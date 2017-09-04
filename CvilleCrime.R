# CvilleTowing
# Nate Day {nathancday@gmail}

#### Globals ------------------------------------------------------------------

setwd("~/future/CvilleTowing/")

library(forecast) # time-series modeling
library(ggsci) # d3 color pallettes
library(forcats) # fct tools
library(lubridate) # date tools
library(magrittr) # piping
library(tidyverse) # data wranglin'

theme_set(theme_minimal())
ang <- theme(axis.text.x = element_text(angle = 45, vjust = .75, hjust = .75))

#### Import -------------------------------------------------------------------

# csv downloaded from http://opendata.charlottesville.org/datasets/crime-data
tib <- read_csv("Crime_Data.csv")
names(tib) %<>% tolower()

# head(tib)
# str(tib)

tib %<>% select(-recordid) %>%
    unite(address, blocknumber, streetname) %>%
    rename_at(vars(contains("reported")), funs(gsub("reported", "", .))) %>%
    mutate(hour = parse_time(hour, format = "%H%M"),
           date = as.Date(date),
           year = year(date),
           month = month(date))

#### Explore -------------------------------------------------------------
range(tib$date) # 5 years
# "2012-08-30" "2017-08-27"

## * ggplot2 helpers ----

# nicely labeler
nicely <- function(breaks) {
    
    values <- as.character(breaks) %>% strsplit("-")
    
    m <- map(values, ~ as.numeric(.[2]) %>%
                 month.abb[.])
    y <- map(values, ~ gsub("20", "", .[1]))
    
    labels <- map2(m, y, ~ paste0(., "\n",
                                 ifelse(. == "Jan",
                                        .y,
                                        "")))
    
    return(labels)
    
} 

# saves from jungling around "date_breaks=" every scale_x_date()
brks <- seq(date("2012-07-01"), date("2018-09-01"), "3 months")

## * seasonal
ggplot(tib, aes(date)) +
    geom_freqpoly(bins = 60) + # 60 months in 5 years
    scale_x_date(breaks = brks, date_minor_breaks = "1 month",
                 labels = nicely, expand = c(0,0)) +
    labs(title = "Charlottesville Police Reports",
         y = "# reports",
         x = NULL,
         caption = "CrimeData via COD [2012-08-30 : 2017-08-27] ")
# hoos doing that spike in Sep - Oct?

# set up tibble for a geom to cover Aug16-Oct31
students_back <- tibble(date = seq(as.Date("2012-09-01"), as.Date("2017-9-01"), length.out = 6),
                        date_min = date - ddays(16), # ~ Aug16
                        date_max = date %m+% months(2) )

ggplot(tib, aes(date)) +
    geom_rect(data = students_back, aes(xmin = date_min, xmax = date_max),
              ymin = 0, ymax = Inf, fill = "#f64617") +
    geom_freqpoly(bins = 60, color = "#0c2345", size = 1) + # 60 months in 5 years
    scale_x_date(breaks = brks, date_minor_breaks = "1 month",
                 labels = nicely, expand = c(0,0),
                 limits = c(date("2012-08-16"), date("2017-07-31"))) +
    labs(title = "Monthyl police report totals",
         subtitle = "Rectangles highlight Aug 16th - Oct 31st",
         x = NULL, y = NULL,
         caption = expression(paste(italic("Hoo"), "'s causing that uptick?", " Data shown 2012-08-16 - 2017-07-31")))

### Look at breakdown of offense labels seasonally
tib %<>% filter(!is.na(offense)) # 1 case

# clean out "-"s and "/"s as the represent subgroups and aliases
tib$offense %<>% gsub("-.*", "", .) %>%
    gsub("/.*", "", .)

# FYI this data is packed with a ton of great tags!!!

tib$offense %<>% as.factor() %>%
    forcats::fct_infreq()

sims_tib <- tibble(offense = round(rexp(nrow(tib), 1/6)))
# as numeric hack to get around long labels 
ggplot(tib, aes(as.numeric(offense), fill = "real data")) +
    geom_bar(alpha = .5) +
    geom_bar(data = sims_tib, aes(fill = "exponential sim"), alpha = .5) +
    scale_fill_brewer(palette = "Set1", name = NULL) +
    scale_x_continuous(breaks = seq(10,50,10), limits = c(0,50)) +
    labs(title = "Top 50 tags compared to expoential distribution (rate = .1)",
         y = "count per tag", x = "tag rank by frequency") +
    theme(legend.position = c(.85, .85))


### Write up style break here


# looking at the top 25 tags
t <- levels(tib$offense)[1:25]

# look at top 16
top16 <- mutate(tib, offense = forcats::fct_infreq(offense)) %>%
    filter(offense %in% levels(offense)[1:16]) %>%
    droplevels()

ggplot(top16, aes(offense)) +
    geom_bar(fill = NA, color = "black") +
    theme(axis.text.x = element_text(angle = 45))

# make better (shorter) names
decode <- c("Larceny", "Assault", "Towed", "Traffic",
            "Vandalism", "Property", "Drugs", "Assist Citizen",
            "Suspicious", "Fraud", "Burglary", "Animal",
            "Runaway", "DUI", "Disorderly", "Missing Person") %>%
    set_names(levels(top16$offense))

# use infreq() again
top16$offense %<>% decode[.] %>%
    forcats::fct_infreq()

# freqpoly version in facets
ggplot(top16, aes(date, color = offense)) +
    geom_rect(data = students_back, aes(xmin = date_min, xmax = date_max),
              ymin = -0, ymax = Inf, color = NA, fill = "grey", alpha = .5) +
    geom_freqpoly(alpha = .75, bins = 60, size = 1) +
    scale_x_date(breaks = seq(date("2013-01-01"), date("2017-01-01"), length.out = 5),
                 date_labels = "'%y", limits = c(date("2012-08-15"), date("2017-08-16"))) +
    scale_color_d3(palette = "category20") +
    facet_wrap(~offense, scales = "free_y") +
    theme(legend.position = "none") +
    labs(title = "Top 16 most reported offenses (collapsed)",
         caption = "Data shown 2012-08-15 : 2017-08-01",
         y = NULL, x = NULL)

#### Towing -------------------------------------------------------------------
# looking at towing more focuse
tow <- filter(top16, offense ==  "Towed")

#### * look closer -----------
# drop 2012 and 2017 bc partial years
tow %<>% filter(!(year %in% c("2012", "2017")))
# drop 2017 
tow %<>% filter(year != "2017")

ggplot(tow, aes(as.numeric(month), fill = as.factor(year))) +
    geom_histogram(bins = 12) +
    scale_x_continuous(breaks = 1:12, labels = month.abb, expand = c(0,0)) +
    viridis::scale_fill_viridis(name = NULL, discrete = TRUE, direction = -1) +
    labs(title = "Monthly tow totals",
         x = NULL, y = NULL,
         caption = "Data shown 2013-01-01 : 2016-12-31")

#### * model -----------------------------------------------
library(forecast)
# reset tow to only include all data
tow <- filter(top16, offense ==  "Towed")

month_tib <- arrange(tow, year, month) %>%
    group_by(year, month) %>%
    tally() %>%
    ungroup() %>%
    slice(-1) # drop Aug 2012 (only 6 cases)

month_vec <- select(month_tib, n) %>%
    unlist()

# build time-series object
myts <- ts(month_vec, start = c(2012, 9), frequency = 12)
# perform seasonal decompositions
decomp_fit <- stl(myts, s.window= "periodic")

plot(decomp_fit)

# separate trend components
trend_fit <- HoltWinters(fit$time.series[,2])
trend_preds <- forecast(trend_fit, 19) %>% # returns Apr17 : Oct18
    as.tibble %>%
    .[[1]]
# data in model
trend_past <- as.numeric(fit$time.series[,2])

trend_tib <- tibble(date = seq(date("2012-09-01"), date("2018-10-01"), by = "1 month"),
                     y = c(trend_past, trend_preds))

# triple exponential - models level, trend, and seasonal components
hw_fit <- HoltWinters(myts)
plot(hw_fit)

month_preds <- forecast(hw_fit, 15) %>% # returns Sep17 : Oct18
    as.tibble %>%
    .[[1]]

next_tib <- tibble(date = seq(date("2017-09-01"), date("2018-11-01"), "1 month"),
                    y = month_preds)

# format month_tib
month_tib %<>% mutate(date = date(paste(year, month, "01", sep = "-"))) %>%
    rename(y = n)
# bolt on AUG17 info for smoothness
aug17 <- month_tib[nrow(month_tib),]
next_tib %<>% bind_rows(aug17, .)

# expand students_back
students_back2 <- tibble(date = seq(as.Date("2012-09-01"), as.Date("2018-09-01"), length.out = 7),
                        date_min = date - ddays(16), # ~ Aug16
                        date_max = date %m+% months(2) )

# format next_tib
next_tib$year <- year(next_tib$date)

# projections plot
ggplot(month_tib, aes(date, y, color = y)) +
    geom_rect(data = students_back2, aes(xmin = date_min, xmax = date_max, y = NULL),
              ymin = -0, ymax = Inf, color = NA, fill = "grey", alpha = .5) +
    geom_path(data = trend_tib, aes(x = date, y = y, color = NULL), linetype = 2) +
    geom_path(size = 1) +
    geom_path(data = next_tib, aes(y = y), size = 2, alpha = .75) +
    viridis::scale_color_viridis(name = NULL, direction = -1) +
    scale_x_date(breaks = brks, labels = nicely, expand = c(0,0),
                 limits = c(date("2012-08-15"), date("2018-11-01"))) +
    labs(title = "Forecasted monthly tows",
         y = NULL,
         x = NULL) +
    theme(legend.position = "none")
    
