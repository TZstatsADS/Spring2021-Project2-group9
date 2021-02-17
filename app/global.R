#--------------------------------------------------------------------
###############################Install Related Packages #######################
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("tibble")) {
  install.packages("tibble")
  library(tibble)
}
if (!require("shinymaterial")) {
  install.packages("shinymaterial")
  library(shinymaterial)
}
if (!require("shinycssloaders")) {
  install.packages("shinycssloaders")
  library(shinycssloaders)
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}
if (!require("sf")) {
  install.packages("sf")
  library(sf)
}
if (!require("RCurl")) {
  install.packages("RCurl")
  library(RCurl)
}
if (!require("tmap")) {
  install.packages("tmap")
  library(tmap)
}
if (!require("rgdal")) {
  install.packages("rgdal")
  library(rgdal)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}
if (!require("plotly")) {
  install.packages("plotly")
  library(plotly)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("viridis")) {
  install.packages("viridis")
  library(viridis)
}
if (!require("htmlwidgets")) {
  install.packages("htmlwidgets")
  library(htmlwidgets)
}
if (!require("dygraphs")) {
  install.packages("dygraphs")
  library(dygraphs)
}
if (!require("xts")) {
  install.packages("xts")
  library(xts)
}
if (!require("tidyr")) {
  install.packages("tidyr")
  library(tidyr)
}
if (!require("gtrendsR")) {
  install.packages("gtrendsR")
  library(gtrendsR)
}
if (!require("lubridate")) {
  install.packages("lubridate")
  library(lubridate)
}
if (!require("wordcloud")) {
  install.packages("wordcloud")
  library(wordcloud)
}
#--------------------------------------------------------------------
###############################Define Functions#######################
data_cooker <- function(df){
  #input dataframe and change the Country/Region column into standard format
  df$Country.Region <- as.character(df$Country.Region)
  df$Country.Region[df$Country.Region == "Congo (Kinshasa)"] <- "Dem. Rep. Congo"
  df$Country.Region[df$Country.Region == "Congo (Brazzaville)"] <- "Congo"
  df$Country.Region[df$Country.Region == "Central African Republic"] <- "Central African Rep."
  df$Country.Region[df$Country.Region == "Equatorial Guinea"] <- "Eq. Guinea"
  df$Country.Region[df$Country.Region == "Western Sahara"]<-"W. Sahara"
  df$Country.Region[df$Country.Region == "Eswatini"] <- "eSwatini"
  df$Country.Region[df$Country.Region == "Taiwan*"] <- "Taiwan"
  df$Country.Region[df$Country.Region== "Cote d'Ivoire"] <-"Côte d'Ivoire"
  df$Country.Region[df$Country.Region == "Korea, South"] <- "South Korea"
  df$Country.Region[df$Country.Region == "Bosnia and Herzegovina"] <- "Bosnia and Herz."
  df$Country.Region[df$Country.Region == "US"] <- "United States of America"
  df$Country.Region[df$Country.Region == "Burma"]<-"Myanmar"
  df$Country.Region[df$Country.Region == "Holy See"]<-"Vatican"
  df$Country.Region[df$Country.Region=="South Sudan"]<-"S. Sudan"
  return(df)
}

data_cooker2 <- function(df){
  df$location <- as.character(df$location)
  df$location[df$location == "Congo (Kinshasa)"] <- "Dem. Rep. Congo"
  df$location[df$location == "Congo (Brazzaville)"] <- "Congo"
  df$location[df$location == "Central African Republic"] <- "Central African Rep."
  df$location[df$location == "Equatorial Guinea"] <- "Eq. Guinea"
  df$location[df$location == "Western Sahara"]<-"W. Sahara"
  df$location[df$location == "Eswatini"] <- "eSwatini"
  df$location[df$location == "Taiwan*"] <- "Taiwan"
  df$location[df$location == "Cote d'Ivoire"] <-"Côte d'Ivoire"
  df$location[df$location == "Korea, South"] <- "South Korea"
  df$location[df$location == "Bosnia and Herzegovina"] <- "Bosnia and Herz."
  df$location[df$location == "United States"] <- "United States of America"
  df$location[df$location == "Burma"]<-"Myanmar"
  df$location[df$location == "Holy See"]<-"Vatican"
  df$location[df$location =="South Sudan"]<-"S. Sudan"
  return(df)
}

data_mortality_cooker <- function(df){
  #input dataframe and change the Country/Region column into standard format
  #df$country <- as.character(df$Country.Region)
  df$country[df$country == "United States"] <- "US"
  return(df)
}

data_transformer <- function(df) {
  #################################################################
  ##Given dataframe tranform the dataframe into aggregate level with
  ##rownames equal to countries name, and colnames equals date
  #################################################################
  #clean the country/regionnames
  df <- data_cooker(df)
  #columns that don't need 
  not_select_cols <- c("Province.State","Lat","Long")
  #aggregate the province into country level
  aggre_df <- df %>% group_by(Country.Region) %>% 
    select(-one_of(not_select_cols)) %>% summarise_all(sum)
  #assign the country name into row names 
  aggre_df <- aggre_df %>% remove_rownames %>% 
    tibble::column_to_rownames(var="Country.Region")
  #change the colume name into date format
  date_name <- colnames(aggre_df)
  #change e.g: "x1.22.20" -> "2020-01-22"
  date_choices <- as.Date(date_name,format = 'X%m.%d.%y')
  #assign column nam
  colnames(aggre_df) <- date_choices
  return(aggre_df)
}

data_transformer2 <- function(df) {
  df <- data_cooker2(df)
  
  df <- df %>% select(location,date,total_vaccinations_per_hundred) %>% fill(total_vaccinations_per_hundred) %>%
    spread(date,total_vaccinations_per_hundred)
  
  aggre_df <- df %>% group_by(location) %>% summarise_all(sum)
  
  aggre_df <- aggre_df %>% remove_rownames %>% 
    tibble::column_to_rownames(var="location")
  
  return(aggre_df)
}
#--------------------------------------------------------------------
###############################Data Preparation#######################
#Data Sources
"Dong E, Du H, Gardner L. An interactive web-based dashboard to track COVID-19 in real time. 
Lancet Inf Dis. 20(5):533-534. doi: 10.1016/S1473-3099(20)30120-1"
#get the daily global cases data from API
#The URLs are too slow, use cached files.
#Cases_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
Cases_URL <- "./csv_data/time_series_covid19_confirmed_global.csv"
global_cases <- read.csv(Cases_URL)

#get the daily global deaths data from API
#Death_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
Death_URL <- "./csv_data/time_series_covid19_deaths_global.csv"
global_death <- read.csv(Death_URL)

#Vaccine_URL <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
Vaccine_URL <- "./csv_data/vaccinations.csv"
global_Vaccine <- read.csv(Vaccine_URL)


#Recovered_URL<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
Recovered_URL<-"./csv_data/time_series_covid19_recovered_global.csv"
global_recovered <-read.csv(Recovered_URL)

#LookUp_Table_URL<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv"
LookUp_Table_URL<-"./csv_data/UID_ISO_FIPS_LookUp_Table.csv"
lookup <-read.csv(LookUp_Table_URL)

#Mortality_URL<-"https://raw.githubusercontent.com/akarlinsky/world_mortality/main/world_mortality.csv"
Mortality_URL<-"./csv_data/world_mortality.csv"
global_mortality <-read.csv(Mortality_URL)

US_vaccine_URL<-"https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv"
US_vaccine<-read.csv(US_vaccine_URL)

####global mortality cleanup
global_mortality_timeseries_cleaned <-data_mortality_cooker(select(global_mortality, country=1,year=year,time=time,unit=time_unit,deaths=deaths))


####lookup table
CountryLookup<-lookup %>% select(iso3,Country_Region)
uni<-CountryLookup[!duplicated(CountryLookup$Country_Region), ]
#country selectorlistbox values
Full_Country_Name_Vec<-c(uni$Country_Region)
ISO_Name_Vec<-c(uni$iso3)
names(ISO_Name_Vec) = Full_Country_Name_Vec

#get aggregate cases 
aggre_cases <- as.data.frame(data_transformer(global_cases))
#get aggregate death
aggre_death <- as.data.frame(data_transformer(global_death))
#define date_choices 
date_choices <- as.Date(colnames(aggre_cases),format = '%Y-%m-%d')
#define country_names
country_names_choices <- rownames(aggre_cases)

aggre_recovered <- as.data.frame(data_transformer(global_recovered))

aggre_vaccination <- as.data.frame(data_transformer2(global_Vaccine))

#Download the spatial polygons dataframe in this link
# https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-admin-0-countries-2/

output_shapefile_filepath <- "./output/countries_shapeFile.RData"

#if already has countries_shapeFile.RData under output folder, no need to process it again
#otherwise, read files from data folder to create countries_shapeFile.RData under output folder
if(file.exists(output_shapefile_filepath)){
  load(output_shapefile_filepath)
}else{
  countries <- readOGR(dsn ="../data/ne_50m_admin_0_countries",
                       layer = "ne_50m_admin_0_countries",
                       encoding = "utf-8",use_iconv = T,
                       verbose = FALSE)
  save(countries, file=output_shapefile_filepath)
}


#make a copy of aggre_cases dataframe
aggre_cases_copy <- as.data.frame(aggre_cases)
aggre_cases_copy$country_names <- as.character(rownames(aggre_cases_copy))

#make a copy of aggre_death dataframe
aggre_death_copy <- as.data.frame(aggre_death)
aggre_death_copy$country_names <- as.character(rownames(aggre_death_copy))

aggre_vaccination_copy <- as.data.frame(aggre_vaccination)
aggre_vaccination_copy$country_names <- as.character(rownames(aggre_vaccination_copy))

binning<- function(x) {10^(ceiling(log10(x)))}

#use save.image() at any time to save all environment data into an .RData file
save.image(file='./output/covid-19.RData')
