
library(shiny)
library(leaflet)
library(htmltools)
library(shinythemes)
tib <- readRDS("tib3.RDS")

shinyUI(fluidPage(
    theme = "united",
  
    titlePanel("Charlottesville Crime Reports: 2016"),
    
  sidebarLayout(
    sidebarPanel(
       selectizeInput("offense", "Offense:", choices = tib$offense,
                      selected = "Towed", multiple = T),
       sliderInput("range", "Range:", as.Date("2016-01-01"), as.Date("2016-12-31"),
                   c(as.Date("2016-01-01"), as.Date("2016-12-31")),
                   timeFormat = "%D"),
       checkboxInput("chloro", "Police Beat Chloropleth?", value = FALSE)
    ),
    
    mainPanel(
       # textOutput("check"),
       leafletOutput("map", height = 600),
       div(hr(),
           p(a("built by Nate",
             href = "https://nateday.me",
             target = "_blank"),
             "with",
           a("data from Charlottesville Open Data Portal",
             href = "http://opendata.charlottesville.org/datasets?t=Public%20Safety",
             target = "_blank") )
       )
    )
  )
))
