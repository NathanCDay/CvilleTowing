
library(shiny)
library(leaflet)
library(htmltools)
library(shinythemes)
tib <- readRDS("tib3.RDS")

shinyUI(fluidPage(
    shinythemes::themeSelector(),
  
    titlePanel("Cville Crime Data : 2016"),
    
  sidebarLayout(
    sidebarPanel(
       selectizeInput("offense", "Offense:", choices = tib$offense,
                      multiple = T),
       sliderInput("range", "Range:", as.Date("2016-01-01"), as.Date("2016-12-31"),
                   c(as.Date("2016-01-01"), as.Date("2016-12-31")),
                   timeFormat = "%D"),
       checkboxInput("chloro", "Police Beat Chloropleth?", value = FALSE)
    ),
    
    mainPanel(
       # textOutput("check"),
       leafletOutput("map", height = 600)
    )
  )
))
