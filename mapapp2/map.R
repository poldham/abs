library(shiny)
library(leaflet)
library(dplyr)

data <- read.csv("map_lakes_patents.csv", stringsAsFactors = FALSE) 
lakes <- data$asciiname

ui <- fluidPage(
  titlePanel("Look up Patents and Literature for Lakes in Kenya"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "lakes", 
        label = "lakes", 
        choices = data$asciiname)
  ),
  mainPanel(leafletOutput("mymap")
    )
  )
 )
#sliderInput(# names of lakes)

server <- function(input, output) {
  output$lakeOutput <- renderTable({
    selectInput("lakeInput", "Lake",
      sort(unique(data$asciiname)),
      selected = "Lake Nakuru")
  })
  filtered <- reactive({
    if (is.null(data$lakeInput)) {
      return(NULL)
    }    
    data %>%
      filter(asciiname == data$lakeInput)
  })
  output$mymap <- renderLeaflet({
    leaflet(data) %>% 
      addTiles() %>%  
      addCircleMarkers(~longitude, ~latitude, popup = data$combined_labels, radius = data$families, fillOpacity = 0.5)
  })
}

shinyApp(ui = ui, server = server) 

# lakes_data <- leaflet(map_lakes_patents) %>%
#   addTiles() %>%  
#   addCircleMarkers(~longitude, ~latitude, popup = map_lakes_patents$combined_labels, radius = map_lakes_patents$families, fillOpacity = 0.5, clusterOptions = markerClusterOptions())
# lakes_data
