#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#Load Required Libraries to complete the code.
library(shiny)
library(leaflet)
library(plotly)
library(dplyr)

#Download Latest Hospital Data
hosp_data <- read.csv("https://opendata.arcgis.com/datasets/1044bb19da8d4dbfb6a96eb1b4ebf629_0.csv", header = TRUE, sep = ",")
#Remove all empty values with "0"
hosp_data <- hosp_data[complete.cases(hosp_data), ]
#Assign color palatte for the markers onn map
pal <- colorFactor(palette = 'Paired', domain = hosp_data$HOSPITAL_TYPE)
#Generate Hospital Map and Legend to visualize the data
hosp_map <- leaflet(hosp_data)%>%
    addTiles(urlTemplate = "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png", attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>')%>%
    addCircleMarkers(lng = ~ï..X, lat = ~Y, radius = 2, color = ~pal(HOSPITAL_TYPE), popup = paste("Name;", hosp_data$HOSPITAL_NAME, "<br>", "Type; ", hosp_data$HOSPITAL_TYPE, "<br>", "Beds; ", hosp_data$NUM_STAFFED_BEDS))

shinyServer(function(input, output, session) {
    
    #Generate Interactive Dropdown Box for user to select Desired State of Interest. NOTE, 'choices = unique(hosp_data$STATE_NAME)' in order to deduplicate STATE_NAME Column
    updateSelectizeInput(session, "stateInput", choices = unique(hosp_data$STATE_NAME), label = "", options = list(placeholder = "Select State"), server = TRUE)
    
    selectedState <- reactive({
        hosp_data[hosp_data$STATE_NAME == input$stateInput, ]
    })
    
    #Create Output Map to visualize on the Main Panel
    output$map_I <- renderLeaflet({
        
        #Call Data from URL
        hosp_data <- read.csv("https://opendata.arcgis.com/datasets/1044bb19da8d4dbfb6a96eb1b4ebf629_0.csv", header = TRUE, sep = ",")
        #Complete Dataset by filling empty Values
        hosp_data <- hosp_data[complete.cases(hosp_data), ]
        #Assign color palette
        pal <- colorFactor(palette = 'Paired', domain = hosp_data$HOSPITAL_TYPE)
        #Call User Selected State
        df <- hosp_data[hosp_data$STATE_FIPS == input$State,]
        #Draw Leaflet Map
        leaflet(hosp_data)%>%
            addTiles(urlTemplate = "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png", attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>')%>%
            addCircleMarkers(lng = ~ï..X, lat = ~Y, radius = .5, opacity = .5)
    })
   #Filter LeafletMap
     observeEvent(input$stateInput, {
        #Call a 'leafletProxy" from the Original Map
         leafletProxy("map_I", data = selectedState())%>%
            #Clear Map
            clearMarkers()%>%
            #Add User Selected State
            addCircleMarkers(lng = ~ï..X, lat = ~Y, radius = 2, color = ~pal(HOSPITAL_TYPE), popup = paste("Name;", hosp_data$HOSPITAL_NAME, "<br>", "Type; ", hosp_data$HOSPITAL_TYPE, "<br>", "Beds; ", hosp_data$NUM_STAFFED_BEDS))%>%
            #Zoom to User Selected State
            fitBounds(~min(ï..X), ~min(Y), ~max(ï..X), ~max(Y))
   })
    #Call Data from URL
    hosp_data <- read.csv("https://opendata.arcgis.com/datasets/1044bb19da8d4dbfb6a96eb1b4ebf629_0.csv", header = TRUE, sep = ",")
    #Complete Dataset by filling empty Values
    hosp_data <- hosp_data[complete.cases(hosp_data), ] 
    
    #Draw Plotly Plot
    plot_I <- plot_ly(data = hosp_data, type = 'scatter', x = ~NUM_LICENSED_BEDS, y = ~NUM_STAFFED_BEDS, color = ~HOSPITAL_TYPE, colors = "Paired", mode = 'markers', size = 5, text = ~paste('State: ', STATE_NAME, '</br> Hospital Name: ', HOSPITAL_NAME, '</br> Staffed Beds: ', NUM_STAFFED_BEDS, '</br> Licensed Beds: ', NUM_LICENSED_BEDS))
    #Assign Layout for Plot
    plot_I <- plot_I %>% layout(title = 'Hospitals in the New England Region', xaxis = list(title = 'Number of Licensed Beds', zeroline = TRUE), yaxis = list(title = 'Number of Staffed Beds'))
    
    #Designation Plotly Output for Main Panel
    output$plot_I <- renderPlotly({
        #Call User Selected State
        req(input$stateInput)
        #Filter Plot by User Selected State
        if (identical(input$stateInput, "")) return(NULL)
        plot_ly(filter(hosp_data, STATE_NAME %in% input$stateInput), x = ~NUM_STAFFED_BEDS, y = ~NUM_LICENSED_BEDS, type = 'scatter', color = ~HOSPITAL_TYPE, colors = "Paired", mode = 'markers', size = 5, text = ~paste('Hospital Name: ', HOSPITAL_NAME, '</br> Staffed Beds: ', NUM_STAFFED_BEDS, '</br> Licensed Beds: ', NUM_LICENSED_BEDS))%>%
            layout(legend = list(x = 0, y = -0.75), xaxis = list(title = 'Number of Staffed Beds', zeroline = TRUE), yaxis = list(title = 'Number of Licensed Beds'))

    })
})
