library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(RColorBrewer)

data <- read.csv("kenya_3000.csv", stringsAsFactors = FALSE)
# establish bounds for the map view
bounds <- read.csv("bounds.csv", stringsAsFactors = FALSE)
# TO DO COLOURS - LOOK AT RSTUDIO WALKTHROUGH. THEN SUMMARY TAB WITH GGPLOT
# I think the problem here is that the colors are inside observe and so get rerun each time. Is the observe part really needed as it destroys each time? Maybe copy to a new app and see
# Create colour palette for categorical variable from RColorBrewer using length of var. Note palette size will vary use RColorBrewer::brewer.pal.info to review.
# kingdomcol <- sort(unique(kenya_3000$kingdom))
# my_palette <- brewer.pal(length(kingdomcol), "Paired")
# factpal <- colorFactor(my_palette, domain = kingdomcol)

ui <- fluidPage(
  titlePanel("Map a sample of GBIF Data"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("kenyaOutput")
    ),
    mainPanel(
      tabsetPanel(type = "tabs", position = "above", 
        tabPanel("Map", leafletOutput("mymap")),
        tabPanel("Table", tableOutput("table"))
  )
      )
    )
  )

server <- function(input, output){
  output$kenyaOutput <- renderUI({
    selectInput(inputId = "kingdominput", "kingdom",
      sort(unique(data$kingdom)),
      selected = "Animalia")
  })
 # use renderLeaflet for elements of the map that don't change.  
  output$mymap <- renderLeaflet({
    leaflet(data) %>% 
      addTiles() %>%
      addCircleMarkers(~longitude, ~latitude, popup = data$species)
  })

  # Use leafletProxy for elements that do change
  observe({
    set <- data %>% 
      filter(data$kingdom %in% input$kingdominput)
    
    leafletProxy("mymap") %>% clearMarkers() %>% 
      addCircleMarkers(lng = set$longitude,
        lat = set$latitude, popup = data$species, radius = 1, weight = 2, opacity = 0.5, fill= TRUE, fillOpacity = 0.2)
  })

  output$table <- renderTable({
    table <- data %>% 
      filter(data$kingdom %in% input$kingdominput) 
  })
}

shinyApp(ui = ui, server = server)
