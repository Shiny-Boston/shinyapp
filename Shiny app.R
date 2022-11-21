library(knitr)
library(kableExtra)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(rgdal)
library(maps)
library(mapdata)
library(tmap)
library(tmaptools)
library(tidycensus)
library(sf)
library(rmapshaper)
library(ggplot2)
library(jsonlite)
library(shiny)
library(shinythemes)

UI <- 
  navbarPage("Boston crime map", collapsible = TRUE, inverse = TRUE, theme = shinytheme("cerulean"),
             tabPanel("lat-lon map",
                      fluidPage(
                        selectInput("crime_repo",label = strong("Location"), choices = crime_repo$Location),
                        plotOutput("plot", width = "800px", height = "800px"),
                        )),
             
             tabPanel("day-of-week table",
                      fluidPage(
                        "This tab includes a table that does not rely on locations. This table describes the crime frequency of different days of a week.",
                       tableOutput("table1")
                       
                      )),
             tabPanel("district differed map")
  )

server <- function(input, output) {
  
  county<-map_data("county")
  
  boston <- subset(county, (subregion == "suffolk")&(region == "massachusetts"))
  map11 <- ggplot(data = boston) + 
    geom_polygon(aes(x = long, y = lat, group = group), fill = "#ffcc99", color = "#33cc33") + coord_fixed(1.3)

  crime_repo<-read.csv("tmpttlexpi6.csv")
  
  crime_repo<-crime_repo[(crime_repo$Lat>40)&(crime_repo$Long< -70),]
  
  bb_bb <- data.frame(
    long = crime_repo$Long, lat = crime_repo$Lat,
    names = crime_repo$INCIDENT_NUMBER, stringsAsFactors = FALSE)
  
  map33 <- map11 + geom_point(data=bb_bb, aes(x=long, y=lat), color = "#000099", size=0.5)
  
  
  boston2<-fromJSON("City_of_Boston_Boundary.geojson")
  
  
  cord<-boston2$features$geometry$coordinates
  
  long<-c()
  lat<-c()
  group<-c()
  
  for (i in 1:114) {
    for (j in 1:nrow(cord[[1]][[1]][[i]])) {
      long<-c(long,cord[[1]][[1]][[i]][j,1])
      lat<-c(lat,cord[[1]][[1]][[i]][j,2])
      group<-c(group,i)
    }
    
  }
  
  boston_cord<-data.frame(long=long,lat=lat,group=group)
  
  map44 <- ggplot(data = boston_cord) + 
    geom_polygon(aes(x = long, y = lat, group = group), fill = "#ffcc99", color = "#33cc33") + coord_fixed(1.3)
  
  output$plot <- renderPlot(map44 + geom_point(data=bb_bb, aes(x=long, y=lat), color = "#000099", size=0.5))
  
  crime_repo$DISTRICT<-replace(crime_repo$DISTRICT, crime_repo$DISTRICT == "", "Unknown")
  
  crimebyweek <- table(crime_repo$DAY_OF_WEEK)
  
  crimebydistrict <- table(crime_repo$DISTRICT)
  
  output$plot <- renderPlot(map44 + geom_point(data=bb_bb, aes(x=long, y=lat), color = "#000099", size=0.5))
  
  output$table1 <- renderTable(crimebyweek)
  


  
  
}

shinyApp(ui = UI, server = server)