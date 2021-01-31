#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

shinyUI(fluidPage(
    
    #Title Panel of Application
    titlePanel(HTML("<h1><center><font size = 15> United States Hospitals</font></cneter></h1")),
    #Sidebar Panel with Dropdown Box of States to Filter to Map and Plot
    sidebarLayout(
        sidebarPanel(
            selectizeInput("stateInput", 'State', choices ="", multiple = 
                               FALSE,
                           options = list(placeholder = 'Please Select State')
            )
        ),
        #Main Panel displaying the leaflet Map and plotly Plot
        mainPanel(fluidRow(leafletOutput("map_I", "100%", 500),plotlyOutput(outputId = "plot_I", "100%", 500)))
    )
))

