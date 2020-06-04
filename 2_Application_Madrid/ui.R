ui <- dashboardPage(
  
  skin = "red",
  dashboardHeader(title = "Madrid Application: Current Data"),
  dashboardSidebar(
    
    selectInput("propertytype", "Property Type", 
                choices = c("Centro Madrid",
                            "Arganzuela",
                            "Chamartin",
                            "Chamberri",
                            "Fuencarral",
                            "Moncloa",
                            "Retiro",
                            "Salamanca",
                            "Tetuan")),
    
    selectInput("colorscalevar", "Color Scale Variable",
                choices = c("Sale Price" = "price",
                            "Square Feet" = "TLA", 
                            
                            "Bedrooms" = "beds", 
                            "Bathrooms" = "baths")),
    
    selectInput("circlesizevar", "Circle Size Variable", 
                choices = c("Square Feet" = "TLA",
                            "Sale Price" = "price",
                            "Bedrooms" = "beds",
                            "Bathrooms" = "baths",
                            "None"= "none")),
    

    
    uiOutput("PriceSlider")
    
    
    
    
    
  ),
  
  dashboardBody(
    fluidRow(box(width= 12, leafletOutput("map"))))
  
  
)







