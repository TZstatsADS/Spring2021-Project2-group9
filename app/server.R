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
library(plyr)
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
        
        
        #make ready cases
        covid_dat_unique_country_timeseries_cases <- global_cases%>% filter(Country.Region == CountryFilter)
        cases_df <- as.data.frame(t(covid_dat_unique_country_timeseries_cases))
        cases_df <- tibble::rownames_to_column(cases_df, "row_names")
        cases_df <- cases_df %>% slice(5:n())
        #change column names
        cases<-select(cases_df, D=row_names, C=V1)
        cases$D <- as.Date(cases$D, format="X%m.%d.%y")
        cases$C <- as.integer(cases$C)
        
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
        
        Labelslist <- c()
        Colorlist <-c()
        if (input$vac == TRUE){
            plt<-plt + geom_line(data = Vacc_use_Country_Filter, aes(x=date, y = people_fully_vaccinated, colour="blue"))
            Labelslist <- c(Labelslist, "Fully Vacinated")
            Colorlist <-c(Colorlist,"blue")
        }
        
        if (input$rec == TRUE){
            plt<-plt + geom_line(data = recovered, aes(x=D, y = R, colour="red"))
            Labelslist <- c(Labelslist, "Recovered")
            Colorlist <-c(Colorlist,"red")
        }
        
        if (input$cas == TRUE){
            plt<-plt + geom_line(data = cases, aes(x=D, y = C, colour="black"))
            Labelslist <- c(Labelslist, "Cases")
            Colorlist <-c(Colorlist,"black")
        }
        
        plt<-plt + labs(
            title = "Israel Timeline",
            subtitle = "Period 2020-2021",
            caption = "Data: Johns Hopkins",
            x = "Date",
            y = "Number of People"
        )
        
        plt + scale_color_manual(name = "Legend",breaks = Colorlist, values =Colorlist , labels = Labelslist)
    })
    
    
    
    
    output$plot3<-renderPlot({
        
        Vaccine_URL <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
        global_Vaccine <- read.csv(Vaccine_URL)
        #data 
        ana_data<-global_Vaccine%>%
            select(location,iso_code,date,total_vaccinations, people_vaccinated)
        ana_data<-ana_data[which(!is.na(ana_data$total_vaccinations)),]
        ana_data<-ana_data[which(!is.na(ana_data$people_vaccinated)),]
        ana_data<-ana_data%>%
            filter(iso_code=="USA" | iso_code=="MEX" | iso_code=="CHN" | iso_code=="GBR" | iso_code=="BRA" | iso_code=="ITA"| iso_code=="ISR")
        ana_data<-as.data.frame(ana_data)
        add_var<-function(a){
          
            x<-sum(a$total_vaccinations)
            y<-sum(a$people_vaccinated)
            return(c(x,y))
        }
        
        plot_data_ana<-ddply(ana_data,.(iso_code),add_var)
        
        # Library
        
        
        plot_data_ana2<-plot_data_ana%>%
            select(iso_code,V1,V2)%>%
            gather(key="variable",value="value",-iso_code)
        
        plot_data_ana2<-as.data.frame(plot_data_ana2)
        ggplot(plot_data_ana2,aes(x=iso_code,y=value))+
            geom_bar(stat="identity",fill="red")
            
        # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
        
       
        if (input$Total == TRUE){
            pltdeath<-ggplot(plot_data_ana)+
               geom_bar(stat="identity",aes(x=iso_code,y=V1))
                
        }
        
        if (input$People == TRUE && input$Total == FALSE){
            pltdeath<-ggplot(plot_data_ana)+
                geom_bar(stat="identity",aes(x=iso_code,y=V2))
                
        }
        if (input$Total == TRUE &&input$People == FALSE){
            pltdeath<-ggplot(plot_data_ana)+
                geom_bar(stat="identity",aes(x=iso_code,y=V1))
            
        }
        
        if (input$People == TRUE && input$Total==TRUE){
            pltdeath<-       ggplot(plot_data_ana)+
                geom_bar(stat="identity",aes(x=iso_code,y=V1,fill="total"))+
                geom_bar(stat="identity",aes(x=iso_code,y=V2,fill="people_vaccinated"))+
                labs(y="Number of vaccines",x="country",title="Total Vaccinations VS. People Vaccinated")
        }
       pltdeath+labs(y="Number of vaccines",x="country",title="Total Vaccinations VS. People Vaccinated")
    })
    
    

    output$plot4<-renderPlot({
        cases<-global_cases[, c(2,389)]
        cases<-cases[cases$Country.Region=="United Kingdom"|cases$Country.Region=="China"|cases$Country.Region=="Brazil"|cases$Country.Region=="Israel"|cases$Country.Region=="Italy"|cases$Country.Region=="Mexico"|cases$Country.Region=="US",]
        add_function<-function(a){
            x<-sum(a$X2.9.21)
            return(x)
        }
        cases<-ddply(cases,.(Country.Region),add_function)
        death<-global_death[, c(2,389)]
        death<-death[death$Country.Region=="United Kingdom"|death$Country.Region=="China"|death$Country.Region=="Brazil"|death$Country.Region=="Israel"|death$Country.Region=="Italy"|death$Country.Region=="Mexico"|death$Country.Region=="US",]
        death<-ddply(death,.(Country.Region),add_function)  
        death_case<-cbind(death, cases[,2])
        
        
        

        if (input$Cases==TRUE && input$Death==TRUE | input$Cases==FALSE && input$Death==FALSE) {    
            plt_death<-ggplot(death_case)+
                geom_bar(stat="identity",aes(x=Country.Region,y=cases[,2],fill="cases"))+
                geom_bar(stat="identity",aes(x=Country.Region,y=V1,fill="death"))+
                labs(y="Number of cases",x="country",title="Cases VS. Death")
        }
        if (input$Cases==TRUE && input$Death==FALSE){
            plt_death<- ggplot(death_case)+
            geom_bar(stat="identity",aes(x=Country.Region,y=cases[,2]))+
            labs(y="Number of cases",x="country",title="Cases")
        }
        
        if (input$Cases==FALSE && input$Death==TRUE){
           plt_death<-ggplot(death_case)+
            geom_bar(stat="identity",aes(x=Country.Region,y=V1))+
            labs(y="Number of death",x="country",title="Death")
        
        }
        plt_death+theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

})
