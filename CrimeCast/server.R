
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
                                "Rate: ", round(chl$density,3))
            return(chl)

        }
        if (!input$chloro){
            # add label last
            tib %<>% mutate(label = paste0(offense, "<br>", address))
            return(tib)
        }
    })
    
    # output$check <- renderPrint(input$map_bounds[c(2,4)] %>% as.numeric() %>% mean)

   
      map_output <- function(lat = "38.0283", lon = "-78.4767") {
          
          if (!input$chloro) {
              
              out <- leaflet(data(), options = leafletOptions(minZoom = 13, maxZoom = 16)) %>%
                addProviderTiles("CartoDB.Positron") %>% 
                addCircleMarkers(color = ~offense_pal(color), weight = 1, radius = 5,
                                opacity = .5, fillOpacity = .15,
                                label = ~map(label, HTML)) %>%
                addLegend("topright", pal = offense_pal, values = ~color,
                        title = "Reported offense")
          }
          if (input$chloro) {
              out <- leaflet(data(), options = leafletOptions(minZoom = 13, maxZoom = 15)) %>%
                    addProviderTiles("CartoDB.Positron") %>%
                    addPolygons(color = "#a5a597", weight = 1, smoothFactor = 0.3, fillOpacity = .5,
                    fillColor = ~pal(log(n+1)),
                    label = ~map(label, HTML)) %>%
                  addLegend("topright", pal = pal, values = ~n, opacity = .5,
                            title = "Total Reports")
          }
          return(out)
      }
      observe({
          hld <- input$chloro
          
          isolate({
              cur_zoom <- input$map_zoom
              cur_lat <- input$map_bounds[c(2,4)] %>% as.numeric() %>% mean
              cur_lon <-input$map_bounds[c(1,3)] %>% as.numeric() %>% mean
              
              if(hld) {
                  new_zoom <- 12
              leafletProxy("map") %>%
                  setView(cur_lat, cur_lon, zoom = new_zoom) }
              
              if (!hld) {
                  leafletProxy("map") %>%
                      setView(cur_lat, cur_lon, zoom = cur_zoom) }
          })
      })
      output$map <- renderLeaflet(map_output())
})
