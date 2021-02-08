#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#ag
#    http://shiny.rstudio.com/
#
# Define UI for application that draws a histogram
library(viridis)
library(dplyr)
library(tibble)
library(tidyverse)
library(shinythemes)
library(sf)
library(RCurl)
library(tmap)
library(rgdal)
library(leaflet)
library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)
library(htmlwidgets) # to use saveWidget function

#load('./output/covid-19.RData')

ui = shiny::htmlTemplate(
  # Index Page
  "www/custom_group9.html",
  
  rec_selector= checkboxInput("rec", "Show Recovered", value = TRUE, width = NULL),
  vac_selector = checkboxInput("vac", "Show Vaccinated", value = TRUE, width = NULL),
  plot2=fluidPage(
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot2")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("SIR", plotOutput("sir"))
      )
    )
    
  )
  
)

