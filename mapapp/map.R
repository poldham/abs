library(shiny)
library(leaflet)
library(dplyr)

data <- read.csv("map_lakes_patents.csv", stringsAsFactors = FALSE) 

ui <- fluidPage(
  titlePanel("Look up Patents and Literature for Lakes in Kenya"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("lakeOutput")
  ),
  mainPanel(leafletOutput("mymap")
    )
  )
 )
#sliderInput(# names of lakes)

server <- function(input, output) {
  output$lakeOutput <- renderUI({
    selectInput("lakeInput", "Lake",
      sort(unique(data$asciiname)),
      selected = "Lake Nakuru")
  })
  filtered <- reactive({
    data %>%
      filter(asciiname == input$lakeInput)
  })
  # this works if main table uiOutput is , so now to get it to render in a map
  #output$mymap <- renderTable({filtered()})
  output$mymap <- renderLeaflet({
    leaflet(data) %>% 
      addTiles() %>%  
      fitBounds(lng1 = bounds$west, lat1 = bounds$north, lng2 = bounds$east, lat2 = bounds$south) %>% 
      addCircleMarkers(~longitude, ~latitude, popup = data$asciiname, radius = 1, fillOpacity = 0.5)
   })
}

shinyApp(ui = ui, server = server) 

# lakes_data <- leaflet(map_lakes_patents) %>%
#   addTiles() %>%  
#   addCircleMarkers(~longitude, ~latitude, popup = map_lakes_patents$combined_labels, radius = map_lakes_patents$families, fillOpacity = 0.5, clusterOptions = markerClusterOptions())
# lakes_data
