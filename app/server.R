#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#-------------------------------------------------App Server----------------------------------
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
#can run RData directly to get the necessary date for the app
#global.r will enable us to get new data everyday
#update data with automated script
source("global.R") 
#load('./output/covid-19.RData')
server = shinyServer(function(input, output) {
    
    
    output$plot2  <- renderPlot({
        
        #store filters
        #CountryFilter<- input$selected_country
        CountryFilter<- "Israel"
        COuntryISOFilter<-"ISR"
        
        
        #make ready recovered
        covid_dat_unique_country_timeseries_recovered <- global_recovered %>% filter(Country.Region == CountryFilter)
        final_df <- as.data.frame(t(covid_dat_unique_country_timeseries_recovered))
        final_df <- tibble::rownames_to_column(final_df, "row_names")
        final_df <- final_df %>% slice(5:n())
        #change column names
        recovered<-select(final_df, D=row_names, R=V1)
        recovered$D <- as.Date(recovered$D, format="X%m.%d.%y")
        recovered$R <- as.integer(recovered$R)
        
        
        
        #make ready vaccines
        Vacc_use <- global_Vaccine %>% 
            select(location,iso_code,date,people_fully_vaccinated,people_fully_vaccinated_per_hundred)
        Vacc_use_Country_Filter<- Vacc_use %>% filter(iso_code == COuntryISOFilter & !is.na(people_fully_vaccinated))
        Vacc_use_Country_Filter$date <- as.Date(Vacc_use_Country_Filter$date, format="%Y-%m-%d")
        
        #create plot
        plt<-ggplot()  
        
        if (input$rec == TRUE){
            plt<-plt + geom_line(data = recovered, aes(x=D, y = R, color = "red"))
        }
        
        if (input$vac == TRUE){
            plt<-plt + geom_line(data = Vacc_use_Country_Filter, aes(x=date, y = people_fully_vaccinated, color = "blue"))
        }
        
        plt<-plt + labs(
            title = "Israel Timeline",
            subtitle = "Period 2020-2021",
            caption = "Data: Johns Hopkins",
            x = "Date",
            y = "Number of People"
        )
        
        plt + scale_color_manual(name = "Legend", values = c("blue" = "blue","red" = "red" ), labels = c("Fully Vacinated","Recovered"))
        
        
        #plt
        plot(plt)
        
        
    })
})
