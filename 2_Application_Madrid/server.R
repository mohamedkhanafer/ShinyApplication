server <- function(input, output, session) {
  
  filteredData <- reactive({
    sale_data[sale_data$price >= input$priceslideroutput[1] & 
                sale_data$price <= input$priceslideroutput[2] & 
                sale_data$property_type == input$propertytype, ]})
  
  
  colorpal <- reactive({
    colorNumeric("YlOrRd", sale_data[sale_data$property_type ==input$propertytype,input$colorscalevar])})
  
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
      
      setView(lat = 40.4182029, lng = -3.706617, zoom = 13.5)})
  
  
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
                                          
                                          input$colorscalevar == "TLA" ~ "Square Feet: ",
                                          
                                          
                                          input$colorscalevar == "beds" ~ "Bedrooms: ",
                                          input$colorscalevar == "baths" ~ "Bathrooms: ",
                                          ),
                                if(input$colorscalevar== "price"){currency(price,digits=0L)}
                                else {eval(as.name(paste(input$colorscalevar)))},
                                '<br>' , case_when(input$circlesizevar == "none"  ~"Square Feet: ",
                                                   input$circlesizevar == "price"  ~"Sale Price:",
                                                   input$circlesizevar == "TLA" ~ "Square Feet: ",
                                                   input$circlesizevar == "beds" ~ "Bedrooms: ",
                                                   input$circlesizevar == "baths" ~ "Bathrooms: "),
                                if(input$circlesizevar== "price"){currency(price,digits=0L)}
                                else if (input$circlesizevar== "none"){TLA}
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
  })}









