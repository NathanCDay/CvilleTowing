
library(shiny)
library(leaflet)
library(htmltools)
library(shinythemes)
tib <- readRDS("tib3.RDS")

shinyUI(bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 20, width = "90%",
                  fluidRow(
                      column(4,selectizeInput("offense", "Offense:", choices = tib$offense,
                                          multiple = T) ),
                      column(4, sliderInput("range", "Range:", as.Date("2016-01-01"),
                                            as.Date("2016-12-31"),
                                            c(as.Date("2016-01-01"),as.Date("2016-12-31")),
                                            timeFormat = "%D") ),
                      column(2, checkboxInput("chloro", "Chloropleth?", value = FALSE) )
                  ),
                  textOutput("check")
    )
  )
)
