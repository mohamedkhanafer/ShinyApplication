ui <- dashboardPage(
  
  skin = "blue",
  dashboardHeader(title = "Lucas County OH Application"),
  dashboardSidebar(
      sidebarMenu(
        menuItem("Plot", tabName = "plot", icon = icon("map"))
      ),
    
    selectInput("propertytype", "Property Value", 
                choices = c("Underpriced Houses",
                            "Overpriced Houses")),
    
    selectInput("colorscalevar", "Color Scale Variable",
                choices = c("Square Feet" = "TLA",
                            "Sale Price" = "price",
                            "Year Built" = "yrbuilt",
                             
                            "Lot Size" = "lotsize",
                            "Stories" = "stories", 
                            "Bedrooms" = "beds", 
                            "Bathrooms" = "baths",
                            "Garage Size"= "garagesqft",
                            "Rooms" = "rooms",
                            "Appraised Value" = "avalue")),
    
    selectInput("circlesizevar", "Circle Size Variable", 
                choices = c("Sale Price" = "price",
                            "Appraised Value" = "avalue",
                            "Lot Size" = "lotsize",
                            "Square Feet" = "TLA",
                            "None"= "none",
                            "Garage Size"= "garagesqft",
                            "Stories" = "stories", 
                            "Bedrooms" = "beds",
                            "Rooms" = "rooms",
                            "Bathrooms" = "baths"
                            )),
    
    sliderInput("yearbuilt", "Year Built", 
                min(sale_data$yrbuilt),
                max(sale_data$yrbuilt),
                value = range(sale_data$yrbuilt), sep =""),
    
    sliderInput("saledate", "Sale Date",
                min(sale_data$sale_date),
                max(sale_data$sale_date),
                value =range(sale_data$sale_date)),
    
    uiOutput("PriceSlider"),
    
    menuItem("Linear Regression", tabName = "regression", icon = icon("dashboard")) 
    
),
  
  dashboardBody(
    fluidRow(box(width= 12, leafletOutput("map"))),
    tabItems(
      tabItem("regression",
              fluidRow(box(width= 12,
                titlePanel("Simple Regression Model to understand Price"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("outcome", label = h3("Outcome"),
                                choices = list("Price" = "price",
                                               "Square Feet" = "TLA"), selected = 1),
                    
                    selectInput("indepvar", label = h3("Explanatory variable"),
                                choices = list("Square Feet" = "TLA",
                                               "Apraised Value" = "avalue",
                                               "Rooms" = "rooms",
                                               "Bedrooms" = "beds",
                                               "Bathrooms" = "baths"), selected = 1)),
                  
                  mainPanel(tabsetPanel(type = "tabs",
                                
                                tabPanel("Plot", plotOutput("scatterplot")), # Plot
                                tabPanel("Distribution", # Plots of distributions
                                         fluidRow(
                                           column(6, plotOutput("distribution1")),
                                           column(6, plotOutput("distribution2")))
                                ),
                                tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
                                tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
                                
                    )
                  )
                ))))
    )))
  
  
  








