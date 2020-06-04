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

sale_data <- read.csv(url("https://gist.githubusercontent.com/mohamedkhanafer/6a682f365d2005155336f177870a52f9/raw/9940294c80a716780b5eae18c831e711fba86076/advancedRscrapped"))
sale_data$property_type <- as.character(sale_data[,"property_type"])
sale_data$address <- as.character(sale_data[,"address"])
sale_data$baths <- as.numeric(sale_data[,"baths"])
sale_data[,"sale_date"] <- as.Date(as.character(sale_data[,"sale_date"]),"%y%m%d")
circle.scaler <- function(x){((x-min(x))/(max(x)-min(x)))*200}

