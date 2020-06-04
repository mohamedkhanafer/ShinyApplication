library(shiny)
library(leaflet)
library(RColorBrewer)
library(formattable)
library(dplyr)
library(shinydashboard)

library(spData)
library(sp)
library(rgdal)
library(dplyr)

data("house")
sale_data <- as.data.frame(house)

# Changing the stories column into numbers
sale_data$stories <- as.character(sale_data[,"stories"])
sale_data$stories[sale_data$stories == "one+half"] <- "1.5"
sale_data$stories[sale_data$stories == "one"] <- "1"
sale_data$stories[sale_data$stories == "two" | sale_data$stories == "bilevel" ] <- "2"
sale_data$stories[sale_data$stories == "two+half"] <- "2.5"
sale_data$stories[sale_data$stories == "three"] <- "3"
sale_data$stories[sale_data$stories == "multilvl"] <- "4"
sale_data$stories <- as.numeric(sale_data[,"stories"])

# Making the date format adequate
names(sale_data)[names(sale_data) == "sdate"] <- "sale_date"
sale_data[,"sale_date"] <- as.Date(as.character(sale_data[,"sale_date"]),"%y%m%d")

# Creating a new column to get Underpriced/Overpriced feature
sale_data["property_type"] <- sale_data$avalue - sale_data$price
sale_data$property_type[sale_data$property_type >0] <- "Underpriced Houses"
sale_data$property_type[sale_data$property_type <0] <- "Overpriced Houses"

# Fixing the coordinates into latitudes and longitudes

changing <- sale_data[, c("lat", "long")]
#changing
nad83_coords <- changing
coordinates(nad83_coords) <- c('lat', 'long')
proj4string(nad83_coords)=CRS("+proj=lcc +lat_0=39.6666666666667 
+lon_0=-82.5 +lat_1=41.7 +lat_2=40.4333333333333 +x_0=155000
+y_0=-70000 +ellps=GRS80 +units=ft +no_defs")
coordinates_deg <- spTransform(nad83_coords,CRS("+init=epsg:4326"))
DF <- as.data.frame(coordinates_deg)
names(DF)[names(DF) == "lat"] <- "longitude"
names(DF)[names(DF) == "long"] <- "latitude"
sale_data <- select(sale_data,-c("s1993","s1994","s1995","s1996", "s1997", "s1998", "long", "lat"))
sale_data$longitude <- DF$longitude
sale_data$latitude <- DF$latitude
circle.scaler <- function(x){((x-min(x))/(max(x)-min(x)))*600}
swiss<-sale_data
