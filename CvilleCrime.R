# CvilleTowing
# Nate Day {nathancday@gmail}

#### Globals ------------------------------------------------------------------

setwd("~/future/CvilleTowing/")

library(lubridate)
library(magrittr)
library(tidyverse)

theme_set(theme_minimal())


#### Ins ----------------------------------------------------------------------

# csv downloaded from http://opendata.charlottesville.org/datasets/crime-data
tib <- read_csv("Crime_Data.csv")
names(tib) %<>% tolower()

head(tib)

tib %<>% select(-recordid) %>%
    unite(address, blocknumber, streetname) %>%
    rename_at(vars(contains("reported")), funs(gsub("reported", "", .))) %>%
    mutate(hour = parse_time(hour, format = "%H%M"),
           date = as.Date(date))

#### Seasonal -----------------------------------------------------------------
range(tib$date) # 5 years
# "2012-08-30" "2017-08-27"

# seasonal trends
ggplot(tib, aes(date)) +
    geom_freqpoly(bins = 60) + # 60 months in 5 years
    scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 month",
                 date_labels = "%b")
# hoos doing that spike in Sep - Oct?

# set up tib for a geom to cover Aug16-Oct31
students_back <- tibble(date = seq(as.Date("2012-09-01"), as.Date("2017-9-01"), length.out = 6),
                        date_min = date - ddays(16), # ~ Aug16
                        date_max = date %m+% months(2) )

ggplot(tib, aes(date)) +
    geom_rect(data = students_back, aes(xmin = date, xmax = date_max),
              ymin = 0, ymax = Inf, fill = "#f64617") +
    geom_freqpoly(bins = 60, color = "#0c2345", size = 1) + # 60 months in 5 years
    scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 month",
                 date_labels = "%b%y") +
    labs(title = "CPD Incident Reports",
         subtitle = "Orange rectangles highlight Aug 16th - Oct 31st",
         x = "MonYr",
         y = "# of requests",
         subtitle = "Hoos causing that uptick?")

### Look at breakdown of offense seasonally
tib$offense %<>% as.factor()
tib %<>% filter(!is.na(offense))

# as numeric hack to get interval to slice at based on arranged data.frame
tib %>%
    count(offense, sort = TRUE) %>%
    mutate(offense = forcats::fct_inorder(offense)) %>%
    ggplot(aes(as.numeric(offense), n)) +
    geom_col() +
    scale_x_continuous(breaks = seq(25,125,25))

tib %>%
    mutate(offense = forcats::fct_infreq(offense))

# looking at the top 20
top10 <- mutate(tib, offense = forcats::fct_infreq(offense)) %>%
    filter(offense %in% levels(offense)[1:10]) %>%
    droplevels()

ggplot(top10, aes(offense)) +
    geom_bar(fill = NA, color = "black") +
    theme(axis.text.x = element_text(angle = 45))

# make better (shorter) names
decode <- c("Towing", "Assault", "Traffic", "Vandalism", "Larceny_other", "Drug",
            "Citizen_Assist", "Suspicious_Activity", "Larceny_vehicle", "Property_Found") %>%
    set_names(levels(top10$offense))
top10$offense %<>% decode[.] %>% forcats::fct_infreq()

# freqpoly version in facets
ggplot(top10, aes(date, color = offense)) +
    geom_rect(data = students_back, aes(xmin = date_min, xmax = date_max),
              ymin = -0, ymax = Inf, color = NA, fill = "grey", alpha = .5) +
    geom_freqpoly(alpha = .5, bins = 60, size = 2) +
    scale_x_date(breaks = seq(date("2013-01-01"), date("2017-01-01"), length.out = 5),
                 date_labels = "%y", limits = c(date("2012-08-15"), date("2017-11-01"))) +
    scale_color_d3() +
    facet_wrap(~offense) +
    theme(legend.position = "none")

#### Towing -------------------------------------------------------------------
# looking at towing more focuse
tow <- filter(top10, offense ==  "Towing")

#### look for yearly trends
tow %<>% mutate(year = lubridate::year(date))

# drop 2012 bc partial year (starts 2012-08-30)
tow %<>% filter(year != "2012")

ggplot(tow, aes(year, fill = ..count..)) +
    geom_bar() +
    viridis::scale_fill_viridis(name = NULL, direction = -1) + # y dis not ggplot2 default yet
    labs(title = "Yearly towing in Charlottesville",
         subtitle = "2017 data stops 2017-08-27",
         y = "# incidents",
         x = NULL,
         caption = "CPD Reported 2013-01-01 : 2017-08-27")

#### Now look for high-towing months
tow %<>% mutate(month = lubridate::month(date, label = T))

# drop 2017
tow %<>% filter(year != "2017")

ggplot(tow, aes(as.numeric(month), fill = as.factor(year))) +
    geom_histogram(bins = 12) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_fill_brewer(name = NULL, palette = "Dark2") +
    labs(title = "Total monthly towing",
         x = NULL,
         y = "# incidents",
         caption = "CPD Reported 2013-01-01 : 2016-12-31")

#### * model yearly increases -----------------------------------------------
library(forecast)

# reset tow to only include all data
tow <- filter(top10, offense ==  "Towing")
# drop august 2012

month_tib <- tow %>% mutate(month = month(date, label = TRUE),
                year = as.factor((year(date)))) %>%
    filter(month!= "Aug" | year != "2012") %>% # drop august 2012
    group_by(month, year) %>%
    tally %>%
    arrange(year, month) %>%
    ungroup()

month_vec <- month_tib %>% select(n) %>% unlist()


myts <- ts(month_vec, start = c(2012, 9), frequency = 12)
plot(myts)

# seasonal decomposition
fit <- stl(myts, s.window= "periodic")
plot(fit)

trend_fit <- HoltWinters(fit$time.series[,2])

trend_pred <- forecast(trend_fit, 15)

trend_plot <- tibble(date = seq(date("2017-09-01"), date("2018-11-01"), length.out = 15),
                     y = trend_pred$mean)

trend_plot %<>% bind_rows(tibble(date = seq(date("2012-09-01"), date("2017-04-01"), length.out = 56),
                                        y = fit$time.series[,2] ), . )

# triple exponential - models level, trend, and seasonal components
fit_hw <- HoltWinters(myts)
plot(fit_hw)
# check params
forecast(fit_hw) %>%
    accuracy()

fit_next <- forecast(fit_hw, 15)

plot_next <- tibble(date = seq(date("2017-09-01"), date("2018-11-01"), length.out = 15),
                    y = fit_next$mean)
# bolt on AUG 2017 info for smoothness
plot_next %<>% bind_rows(filter(tow, month == "Aug", year == "2017") %>%
                              summarise(date = date("2017-08-01"),
                                        y = n() ), . )

# plot forcast
month_tib %<>% mutate(date = paste(year, as.numeric(month), "01", sep = "-"))
month_tib$date %<>% as.Date()

# expand students_back
students_back2 <- tibble(date = seq(as.Date("2012-09-01"), as.Date("2018-09-01"), length.out = 7),
                        date_min = date - ddays(16), # ~ Aug16
                        date_max = date %m+% months(2) )

# projections plot
ggplot(month_tib, aes(date, n, color = n)) +
    geom_rect(data = students_back2, aes(xmin = date_min, xmax = date_max, y = NULL),
              ymin = -0, ymax = Inf, color = NA, fill = "grey", alpha = .5) +
    geom_path(data = trend_plot, aes(x = date, y = y, color = NULL), linetype = 2) +
    geom_path(size = 1) +
    geom_path(data = plot_next, aes(y = y, color = y), size = 2, alpha = .75) +
    viridis::scale_color_viridis(name = "# tows", direction = -1) +
    scale_x_date(breaks = seq(date("2013-01-01"), date("2018-09-01"), length.out = 10),
                 date_labels = "%b '%y", limits = c(date("2012-08-15"), date("2018-11-01"))) +
    labs(title = "Towing forecast",
         y = "# incidents",
         x = NULL)

ggplot(trend_plot, aes(date, y)) +
    geom_line()

                

