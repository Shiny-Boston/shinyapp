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




#setwd("D:/Rresources")


## Create a precise map of Boston



boston<-fromJSON("City_of_Boston_Boundary.geojson") #This data comes from https://data.boston.gov/dataset/city-of-boston-boundary

cord<-boston$features$geometry$coordinates  # Generate original data of coordinates of Boston boundary

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


boston_cord_p1<-data.frame(long=long,lat=lat,group=group)  #Get clean coordinates data for the first part of Boston

part2<-as.numeric((cord[[1]][[2]]))

boston_cord_p2<-data.frame(long=part2[1:(length(part2)/2)],lat=part2[(length(part2)/2+1):length(part2)],group=rep(115,length(part2)/2)) # Second part of Boston

boston_cord_cp<-rbind(boston_cord_p1,boston_cord_p2) # Combine these two parts

map1<- ggplot(data = boston_cord_cp) +            
  geom_polygon(aes(x = long, y = lat, group = group), fill = "#FFFFFF" , color = "#33cc33") + coord_fixed(1.3)

map1 # A precise map of Boston


## Then we add points on the map, where crimes happened


crime_repo<-read.csv("Boston2022crime.csv") # This is CRIME INCIDENT REPORTS in Boston in 2022. Download from: https://data.boston.gov/dataset/6220d948-eae2-4e4b-8723-2dc8e67722a3/resource/313e56df-6d77-49d2-9c49-ee411f10cf58/download/tmpdg9fc0p7.csv


crime_repo<-crime_repo[(crime_repo$Lat > 40)&(crime_repo$Lat < 42.45)&(crime_repo$Long < -70)&(crime_repo$Long > -71.2),] # We omit missing values in Lat and Long and also ignore the crimes that are far away from Boston(only one case for some unknown reason).


crime_repo$DISTRICT<-replace(crime_repo$DISTRICT, crime_repo$DISTRICT == "", "Unknown") # We checked the "DISTRICT" column and found some missing values, we replace these by "Unknown“


crime_position <- data.frame(         
  long = crime_repo$Long, lat = crime_repo$Lat,
  names = crime_repo$INCIDENT_NUMBER, 
  district = crime_repo$DISTRICT,
  stringsAsFactors = FALSE)

map2 <- map1 + geom_point(data=crime_position, aes(x=long, y=lat), color = "#000099", size=0.5)

map2 #Boston map with crimes points on it

#We can look into which district these crimes belong to and mark the positions on the map separately

levels(as.factor(crime_position$district))

distri <- levels(as.factor(crime_position$district))

# We can see there are total 14 districts in this data

# For example, the district "A1":

map2_A1 <- map1 + geom_point(data=crime_position[crime_position$district == "A1",], aes(x=long, y=lat), color = "#000099", size=0.5)

map2_A1

# The district "Unknown":

map2_Unknown <- map1 + geom_point(data=crime_position[crime_position$district == "Unknown",], aes(x=long, y=lat), color = "#000099", size=0.5)

map2_Unknown

# The district “External”

map2_External <- map1 + geom_point(data=crime_position[crime_position$district == "External",], aes(x=long, y=lat), color = "#000099", size=0.5)

map2_External



## Next let's show a table that contains the info on which weekdays the crimes happened


day_of_week<-as.factor(crime_repo$DAY_OF_WEEK)

day_of_week<-factor(day_of_week,levels = levels(factor(1:7,labels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))))



####################
###Shiny App



UI <- 
  navbarPage("Boston crime map", collapsible = TRUE, inverse = TRUE, theme = shinytheme("cerulean"),
             tabPanel("lat-lon map",
                      fluidPage(
                        "Swipe the slider to mark your point of interest on the map",
                        sliderInput("userlong", "Longitude", value = -71.1, min = -71.2, max = -70.95),
                        sliderInput("userlat", "Latitude", value = 42.3, min = 42.2, max = 42.4),
                        "Please choose radius",
                        sliderInput("userr", "Interested Radius", value = 0.02, min = 0, max = 0.12),
                        "The bule points represent crimes location",
                        textOutput("text"),
                        plotOutput("plot", width = "500px", height = "500px")
                      )),
             
             tabPanel("day-of-week table",
                      fluidPage(
                        "This tab includes a table that does not rely on locations. This table describes the crime frequency of different days of a week.",
                       tableOutput("table1")
                       
                      )),
             tabPanel("district differed map",fluidPage(
               "Please select a disrict you interested in",
               selectInput("userdistrict", label = "District", choices = distri),
               plotOutput("plot3", width = "400px"),
               "The plot above shows you the distribution of crime locations in your selected district"
             ) )
  )

server <- function(input, output, session) {


  

  
  crimebyweek <- table(day_of_week)
  
  
  output$table1 <- renderTable(crimebyweek)
  
  
  output$plot <- renderPlot(map2+geom_point(aes(x=input$userlong, y=input$userlat), color = "red", size=3)+
                              annotate("path", color = "red", size = 1,
                                       x=input$userlong+input$userr*cos(seq(0,2*pi,length.out=100)),
                                       y=input$userlat+input$userr*sin(seq(0,2*pi,length.out=100)))
                            , res = 96)
  output$text <- renderText({ 
    num_crime <- sum(((crime_position$long-input$userlong)^2+(crime_position$lat-input$userlat)^2)<=input$userr^2)
    paste0("The number of crimes within your chosen area is ", num_crime)
  })
  
  
  
  output$plot3 <- renderPlot(map1+geom_point(data=crime_position[crime_position$district == input$userdistrict,], aes(x=long, y=lat), color = "#000099", size=0.5), res = 96)


  
  
}

shinyApp(ui = UI, server = server)
