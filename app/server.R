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
library(dygraphs)
library(tidyr)
library(xts)
library(gtrendsR)
#can run RData directly to get the necessary date for the app
#global.r will enable us to get new data everyday
#update data with automated script
source("global.R") 
#load('./output/covid-19.RData')

server = shinyServer(function(input, output) {
    
    
    output$plot2  <- renderPlot({

        #store filters
        CountryFilter<- names(ISO_Name_Vec)[ISO_Name_Vec == input$select_country]
        COuntryISOFilter<-input$select_country
        
        
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
            title = paste(CountryFilter, "Timeline", sep =" "),
            subtitle = "Period 2020-2021",
            caption = "Data: Johns Hopkins",
            x = "Date",
            y = "Number of People"
        )

        plt + scale_color_manual(name = "Legend",breaks = Colorlist, values =Colorlist , labels = Labelslist)
    })
    
    output$vaccine_interest_timeline = renderDygraph({
        geo=""
        if(input$search_geo == "World")
            geo=""
        else if(input$search_geo == "United States")
            geo="US"

        # TODO This is slow live - maintain data locally
        trends = gtrends(c("Moderna", "BioNTech", "AstraZeneca", "Pfizer"), #"Novavax", 
                      geo=geo,
                      gprop="web",
                      time="today 12-m")
        data = trends$interest_over_time %>%
            mutate(hits = replace(hits, hits == "<1", "0")) %>%
            mutate(hits = as.numeric(hits)) %>%
            pivot_wider(id_cols=date, names_from=keyword, values_from=hits)
        data = as.xts(data, order.by = data$date)
        data = data[, colnames(data) != "date"]
        
        dygraph(data) %>%
            dyRangeSelector()
    })
    
    
    
    
    data_countries <- reactive({
    if(!is.null(input$choices)){
      if(input$choices == "cases"){
        return(aggre_cases_copy)
        
      }else{
        return(aggre_death_copy)
      }}
  })
  
  #get the largest number of count for better color assignment
  maxTotal<- reactive(max(data_countries()%>%select_if(is.numeric), na.rm = T))    
  #color palette
  pal <- reactive(colorNumeric(c("#FFFFFFFF" ,rev(inferno(256))), domain = c(0,log(binning(maxTotal())))))    
  
  output$map <- renderLeaflet({
    map <-  leaflet(countries) %>%
      addProviderTiles("Stadia.Outdoors", options = providerTileOptions(noWrap = TRUE)) %>%
      setView(0, 30, zoom = 3) })
  
  
  observe({
    if(!is.null(input$date_map)){
      select_date <- format.Date(input$date_map,'%Y-%m-%d')
    }
    if(input$choices == "cases"){
      #merge the spatial dataframe and cases dataframe
      aggre_cases_join <- merge(countries,
                                data_countries(),
                                by.x = 'NAME',
                                by.y = 'country_names',sort = FALSE)
      #pop up for polygons
      country_popup <- paste0("<strong>Country: </strong>",
                              aggre_cases_join$NAME,
                              "<br><strong>",
                              "Total Cases: ",
                              aggre_cases_join[[select_date]],
                              "<br><strong>")
      leafletProxy("map", data = aggre_cases_join)%>%
        addPolygons(fillColor = pal()(log((aggre_cases_join[[select_date]])+1)),
                    layerId = ~NAME,
                    fillOpacity = 1,
                    color = "#BDBDC3",
                    weight = 1,
                    popup = country_popup) 
    } else {
      #join the two dfs together
      aggre_death_join<- merge(countries,
                               data_countries(),
                               by.x = 'NAME',
                               by.y = 'country_names',
                               sort = FALSE)
      #pop up for polygons
      country_popup <- paste0("<strong>Country: </strong>",
                              aggre_death_join$NAME,
                              "<br><strong>",
                              "Total Deaths: ",
                              aggre_death_join[[select_date]],
                              "<br><strong>")
      
      leafletProxy("map", data = aggre_death_join)%>%
        addPolygons(fillColor = pal()(log((aggre_death_join[[select_date]])+1)),
                    layerId = ~NAME,
                    fillOpacity = 1,
                    color = "#BDBDC3",
                    weight = 1,
                    popup = country_popup)
      
    }
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
        
        
        library("tidyverse")
        plot_data_ana2<-plot_data_ana%>%
            select(iso_code,V1,V2)%>%
            gather(key="variable",value="value",-iso_code)
        
        plot_data_ana2<-as.data.frame(plot_data_ana2)
        ggplot(plot_data_ana2,aes(x=iso_code,y=value))+
            geom_bar(stat="identity",fill="red")
            
        # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
       
        ggplot(plot_data_ana)+
            geom_bar(stat="identity",aes(x=iso_code,y=V1,fill="total"))+
            geom_bar(stat="identity",aes(x=iso_code,y=V2,fill="people_vaccinated"))+
            labs(y="Number of vaccines",x="country",title="Total Vaccinations VS. People Vaccinated")
        
       
    })
    
   # output$plot4<-renderPlot({
   #     Death_data<-global_death[,-c(3, 1, 4)]
   #     Death_data<-Death_data%>%
   #         filter(Country.Region=="United Kingdom"|Country.Region=="US"|Country.Region=="Mexico"|Country.Region=="Italy"
   #                |Country.Region=="Israel"|Country.Region=="Brazil"|Country.Region=="China")
   #
    #    apply(Death_data[-1,-1],1,sum)
   #     dim(Death_data)
            
   # })
    
})
