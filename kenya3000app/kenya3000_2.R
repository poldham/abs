library(shiny)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(data.table)

# use datatable to speed things up here
data <- fread("kenyaocc1.csv", na.strings = c("", NA))

# establish bounds for map view
bounds <- read.csv("bounds.csv", stringsAsFactors = FALSE)

# function to create hyperlinks
map_url <- function(query, label = "NULL", type = "NULL"){
  href <- "<a href="
  close_href <- ">" #included for flexibility in labelling
  close_a <- "</a>"
  if(type == "google"){
    query <- stringr::str_replace_all(query, " ", "+") 
    google_base <- "https://www.google.co.uk/#q="
    url <- paste0(google_base, query)
    out <- paste0(href, shQuote(url), close_href, label, close_a)
  }
  if(type == "crossref"){
    query <- stringr::str_replace_all(query, " ", "+%2B")
    crossref_base <- "http://search.crossref.org/?q=%2B"
    url <- paste0(crossref_base, query)
    out <- paste0(href, shQuote(url), close_href, label, close_a)
  }
  if(type == "gbif"){
    query <- stringr::str_replace_all(query, " ", "+")
    gbif_base <- "http://www.gbif.org/species/search?q="
    url <- paste0(gbif_base, query)
    out <- paste0(href, shQuote(url), close_href, label, close_a)
  }
  if(type == "lens"){
    # note restriction to main jurisdictions and no stemming to reduce duplication and false positives
    query <- stringr::str_replace_all(query, " ", "+")
    lens_base <- "https://www.lens.org/lens/search?q="
    url <- paste0(lens_base, "%22", query, "%22", "&jo=true&j=EP&j=JP&j=US&j=WO&st=false&n=50")
    out <- paste0(href, shQuote(url), close_href, label, close_a)
  }
  out
}

# create columns with formatted links
data$google <- map_url(data$species, label = "Lookup Google", type = "google")
data$crossref <- map_url(data$species, label = "Lookup Crossref", type = "crossref")
data$lens <- map_url(data$species, label = "Lookup Patents", type = "lens")
data$gbif <- map_url(data$species, label = "Lookup GBIF", type = "gbif")

# combine links for use as popup in leafletproxy

data$combined_label <- paste0("<br>", "<strong>", data$species, "</strong>", "</br>", "<br>", data$google, "</br>", "<br>", data$gbif, "</br>", "<br>", data$crossref, "</br>", "<br>", data$lens, "</br>")

# create color palette
kingdom <- sort(unique(kenya_3000$kingdom))
my_palette <- brewer.pal(length(kingdom), "Paired")
factpal <- colorFactor(my_palette, domain = kingdom, ordered = TRUE)

# user interface

ui <- fluidPage(
  titlePanel("Map a sample of GBIF Data"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("kenya_output")
    ),
    mainPanel(
      tabsetPanel(type = "tabs", position = "above", 
        tabPanel("Map", leafletOutput("mymap")),
        tabPanel("Summary", plotOutput("summary")),
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
)

server <- function(input, output){
  output$kenya_output <- renderUI({
    selectInput(inputId = "kingdom_input", "kingdom",
      sort(unique(data$kingdom)),
      selected = "Animalia")
  })
  # use renderLeaflet for elements of the map that don't change, note setting default sizes 
  output$mymap <- renderLeaflet({
    leaflet(data) %>% 
      addTiles() %>%
      addCircleMarkers(~longitude, ~latitude, popup = data$species, radius = 1, weight = 2, opacity = 0.5, fill= TRUE, fillOpacity = 0.2)
  })
  
  # Use leafletProxy for elements that change
  observe({
    set <- data %>% 
      filter(data$kingdom %in% input$kingdom_input)
    
    leafletProxy("mymap") %>% clearMarkers() %>% 
      addCircleMarkers(lng = set$longitude,
        lat = set$latitude, popup = data$combined_label, radius = 1, weight = 2, opacity = 0.5, fill= TRUE, fillOpacity = 0.2, color = factpal(input$kingdom_input))
  })
  output$table <- renderDataTable({
    table <- data %>% 
      filter(data$kingdom %in% input$kingdom_input) 
  })
# Add summary plot counting species by occurrences. filter(n > 10) to avoid swamping the chart.
  #Note this needs an input as it is just showing the overall summary at the moment.
  output$summary <- renderPlot({
    data %>% 
      filter(data$kingdom %in% input$kingdom_input) %>% 
      filter(taxonrank == "SPECIES") %>% 
      count(species) %>%
      dplyr::filter(n > 10) %>% 
      ggplot(aes(x = reorder(species, n), y = n, fill = species)) +
      geom_bar(stat="identity", show.legend = FALSE) +
      coord_flip() + labs(x = "species", y = "occurrences") 
  })
}

shinyApp(ui = ui, server = server)
