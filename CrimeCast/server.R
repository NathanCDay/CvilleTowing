
library(shiny)
library(leaflet)
library(htmltools)
library(magrittr)
library(tidyverse)

# data
tib <- readRDS("tib3.RDS")
chl <- readRDS("cville_json.RDS")
# pallettes
b8 <- scales::brewer_pal(palette = "Dark2")(8)
offense_pal <- colorFactor(palette = b8,
                           domain = unique(tib$color))
pal <- colorNumeric("viridis", NULL)

shinyServer(function(input, output) {
    
    data <- reactive({
        req(input$offense)
        tib %<>% filter(offense %in% input$offense) %>%
            filter()
        
        req(input$range)
        tib %<>% filter(date %in% seq(input$range[1], input$range[2], by = "day"))
        
        tib$NAME %<>% as.factor()
        
        if (input$chloro == TRUE) {
            tib_sum <- group_by(tib, NAME) %>% count() %>%
                complete(NAME, fill = list(n = 0)) %>% unique
            chl$n <- tib_sum$n
            chl$density <- chl$n / (chl$POPULATION+1)
            chl$label <- paste0(chl$NAME, "<br>",
                                "Pop: ", formatC(chl$POPULATION, big.mark = ","), "<br>",
                                "Reports: ", chl$n, "<br>",
                                "Rate: ", round(chl$density,2))
            return(chl)

        }
        if (!input$chloro){
            # add label last
            tib %<>% mutate(label = paste0(offense, "<br>", address))
            return(tib)
        }
    })
    
    # output$check <- renderText(class(input$chloro))
   
      map_output <- function() {
          
          if (!input$chloro) {
              out <- leaflet(data(), options = leafletOptions(minZoom = 12, maxZoom = 15)) %>%
                setView("-78.4767", "38.0293", 13) %>%
                addProviderTiles("CartoDB.Positron") %>% 
                addCircleMarkers(color = ~offense_pal(color), weight = 1, radius = 5,
                                opacity = .5, fillOpacity = .15,
                                label = ~map(label, HTML)) %>%
                addLegend("bottomright", pal = offense_pal, values = ~color,
                        title = "Reported offense")
          }
          if (input$chloro) {
              out <- leaflet(data(), options = leafletOptions(minZoom = 12, maxZoom = 17)) %>%
                    addProviderTiles("CartoDB.Positron") %>%
                    addPolygons(color = "#a5a597", weight = 1, smoothFactor = 0.3, fillOpacity = .5,
                    fillColor = ~pal(n),
                    label = ~map(label, HTML))
          }
          return(out)
      }
      output$map <- renderLeaflet(map_output())
})
