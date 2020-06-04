server <- function(input, output, session) {
  
  filteredData <- reactive({
    sale_data[sale_data$price >= input$priceslideroutput[1] & 
                sale_data$price <= input$priceslideroutput[2] & 
                sale_data$property_type == input$propertytype &
                sale_data$yrbuilt >= input$yearbuilt[1] & 
                sale_data$yrbuilt <= input$yearbuilt[2] & 
                sale_data$sale_date >= input$saledate[1] & 
                sale_data$sale_date <= input$saledate[2], ]})
  
  
  colorpal <- reactive({
    colorNumeric("YlGnBu", sale_data[sale_data$property_type ==input$propertytype,input$colorscalevar])})
  
  output$PriceSlider <- renderUI({
    sliderInput("priceslideroutput", "Sale Price", min(sale_data[sale_data$property_type ==input$propertytype,"price"]), 
                max(sale_data[sale_data$property_type ==input$propertytype,"price"]), 
                range(sale_data[sale_data$property_type ==input$propertytype,"price"]))})
  
  
  
  output$map <- renderLeaflet({
    leaflet(sale_data) %>% 
      
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>% 
      
      #addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      #addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      #addLayersControl(
      #baseGroups = c("CartoDB", "Toner", "Toner Lite"),
      #options = layersControlOptions(collapsed = TRUE))
      setView(lat = 41.671810, lng = -83.573252, zoom=11.5)
      })
  
  
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>% clearShapes() %>%
      addCircles(radius = ~if(input$circlesizevar=="none"){80}
                 else{circle.scaler(eval(as.name(paste(input$circlesizevar))))}, 
                 weight = 0.2, 
                 color = "#777777", 
                 fillColor = ~pal(sale_data[sale_data$property_type ==input$propertytype,input$colorscalevar]),
                 fillOpacity = 0.6, 
                 popup = ~paste(case_when(input$colorscalevar == "price"  ~"Sale Price: ",
                                          input$colorscalevar == "yrbuilt" ~"Year Built: ",
                                          input$colorscalevar == "TLA" ~ "Square Feet: ",
                                          input$colorscalevar == "lotsize" ~ "Lot Size: ",
                                          input$colorscalevar == "stories" ~ "Stories: ",
                                          
                                          input$colorscalevar == "beds" ~ "Bedrooms: ",
                                          input$colorscalevar == "baths" ~ "Bathrooms: ",
                                          input$colorscalevar == "garagesqft" ~ "Garage Size: ",
                                          input$colorscalevar == "rooms" ~ "Rooms",
                                          input$colorscalevar ==  "avalue" ~ "Appraised Value"),
                                if(input$colorscalevar== "price"){currency(price,digits=0L)}
                                else {eval(as.name(paste(input$colorscalevar)))},
                                '<br>' , case_when(input$circlesizevar == "none"  ~"Apraised Value: ",
                                                   input$circlesizevar == "price"  ~"Sale Price:",
                                                   input$circlesizevar == "yrbuilt" ~"Year Built: ",
                                                   input$circlesizevar == "TLA" ~ "Square Feet: ",
                                                   input$circlesizevar == "lotsize" ~ "Lot Size: ",
                                                   input$circlesizevar == "stories" ~ "Stories: ",
                                                   
                                                   input$circlesizevar == "beds" ~ "Bedrooms: ",
                                                   input$circlesizevar == "baths" ~ "Bathrooms: ",
                                                   input$colorscalevar == "garagesqft" ~ "Garage Size: ",
                                                   input$colorscalevar == "rooms" ~ "Rooms",
                                                   input$colorscalevar ==  "avalue" ~ "Appraised Value"),
                                if(input$circlesizevar== "price"){currency(price,digits=0L)}
                                else if (input$circlesizevar== "none"){currency(avalue,digits=0L)}
                                else {eval(as.name(paste(input$circlesizevar)))}
                 )
      )})
  
  observe({
    proxy <- leafletProxy("map", data = sale_data)
    proxy %>% clearControls()
    pal <- colorpal()
    proxy %>% addLegend(position = "bottomleft", pal = pal,
                        values = ~sale_data[sale_data$property_type ==input$propertytype,input$colorscalevar ], 
                        title = case_when(input$colorscalevar == "price" ~"Sale Price",
                                          input$colorscalevar == "yrbuilt" ~ "Year Built",
                                          input$colorscalevar == "TLA" ~ "Square Feet",
                                          input$colorscalevar == "lotsize" ~ "Lot Size",
                                          input$colorscalevar == "stories" ~ "Stories",
                                          
                                          input$colorscalevar == "beds" ~ "Bedrooms: ",
                                          input$colorscalevar == "baths" ~ "Bathrooms: ",
                                          input$colorscalevar == "garagesqft" ~ "Garage Size: ",
                                          input$colorscalevar == "rooms" ~ "Rooms",
                                          input$colorscalevar ==  "avalue" ~ "Appraised Value"))  
  })
  
  
  
  # Regression output
  output$summary <- renderPrint({
    fit <- lm(sale_data[,input$outcome] ~ sale_data[,input$indepvar])
    
    names(fit$coefficients) <- c("Intercept", input$var2)
    
    summary(fit)
  })
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(sale_data[,c('yrbuilt','price','TLA','beds','baths' )], 
                  options = list(lengthChange = FALSE))
  })
  
  
  # Scatterplot output
  output$scatterplot <- renderPlot({
    
    plot(sale_data[,input$indepvar], sale_data[,input$outcome], main="Scatterplot",
         xlab=input$indepvar, ylab=input$outcome, pch = 21, bg = "lightgray", col = "black", 
         lwd = 0.9, cex = 1.5)
    
    abline(lm(sale_data[,input$outcome] ~ sale_data[,input$indepvar]), col="red")
    
    lines(lowess(sale_data[,input$indepvar],sale_data[,input$outcome]), col="blue")
  }, height=400)
  
  
  # Histogram output var 1
  output$distribution1 <- renderPlot({
    
    hist(sale_data[,input$outcome], main="", xlab=input$outcome, col="lightgray", breaks=6)
    
  }, height=300, width=200)
  
  # Histogram output var 2
  output$distribution2 <- renderPlot({
    
    hist(sale_data[,input$indepvar], main="", xlab=input$indepvar, col="lightgray", breaks=6)
    
  }, height=300, width=200)
}









