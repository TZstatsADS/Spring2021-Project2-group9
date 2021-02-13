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
library(dygraphs)
library(tidyr)
library(xts)
library(gtrendsR)

#load('./output/covid-19.RData')

ui = shiny::htmlTemplate(
  # Index Page
  "www/custom_group9.html",
  
  # Vaccine timeline
  rec_selector= checkboxInput("rec", "Show Recovered", value = TRUE, width = NULL),
  vac_selector = checkboxInput("vac", "Show Vaccinated", value = TRUE, width = NULL),
  cas_selector = checkboxInput("cas", "Show Cases", value = TRUE, width = NULL),

  total_selector= checkboxInput("Total", "Show total Vaccination", value = TRUE, width = NULL),
  people_selector = checkboxInput("People", "Show People Vaccinated", value = TRUE, width = NULL),
  
  Death_selector= checkboxInput("Death", "Show # of death", value = TRUE, width = NULL),
  Cases_selector = checkboxInput("Cases", "Show # of cases confirmed", value = TRUE, width = NULL),
  
  # Vaccine search interest
  search_geo_selector = selectInput("search_geo", "Select Search Locality",
                                    choices = c("World", "United States"), selected = "World"),
  
  time_selector = sliderInput('date_map','Input Date:',
                #first day of data recording
                min = as.Date(date_choices[1]),
                #present day of data recording
                max = as.Date(tail(date_choices,1)),
                value = as.Date('2020-04-01','%Y-%m-%d'),
                timeFormat = "%Y-%m-%d",
                animate = TRUE, step = 5),
  
  country_selector=selectInput("select_country", label = h3("Select Country"), 
                               choices = ISO_Name_Vec, 
                               selected = "ISR"),
  
  map_choices=selectInput('choices','Which data to visualize:',
                          choices = c('Cases','Death'),
                          selected = c('Cases'))
  
  
  ##leaflet_map = leafletOutput(outputId = "map")
  
)



