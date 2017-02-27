library(shiny)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(data.table)

# use datatable to speed things up here
data <- fread("kenya_slim.csv", na.strings = c("", NA))

data <- sample_n(data, 30000)

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

# create stable color palette
kingdom <- c("incertae sedis", "Archaea", "Fungi", "Plantae", "Chromista", "Animalia", "Bacteria", "Protozoa", "Viruses")
# my_palette <- brewer.pal(length(kingdom), "Paired")
load("my_palette.rda")
factpal <- colorFactor(my_palette, domain = kingdom, ordered = TRUE)

# user interface

ui <- fluidPage(
  titlePanel("Map a sample of GBIF Data"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("kenya_output"),
      br(),
      downloadButton("downloadData", "csv Download"),
      br(),
      br(),
      h3("About"),
      p("This app provides an example of the visualisation of 30,000 species occurrence records for Kenya from the", a("Global Biodiversity Information Facility.", href="http://www.gbif.org/", target = "_blank"), "To learn how the data was created see", a("this article.", href="https://poldham.github.io/abs/mapgbif.html", target = "_blank")),
      p("The raw data can be accessed from the following DOI and please cite as: GBIF.org (24th January 2017) GBIF Occurrence Download", a("http://doi.org/10.15468/dl.b04fyt", href="http://doi.org/10.15468/dl.b04fyt", target = "_blank"),". The data was imported using", a("rgbif from ROpensci", href="https://github.com/ropensci/rgbif"), "and the site was built in", a("RStudio", href="https://www.rstudio.com/", target = "_blank"), "with", a("Shiny.", href="https://www.rstudio.com/products/shiny/", target = "_blank")),
      p("The app does not use Cookies.")
    ),
    mainPanel(width = 8,
      tabsetPanel(type = "tabs",  
        tabPanel("Map", leafletOutput("mymap")),
        tabPanel("Summary", plotOutput("summary")),
        tabPanel("Table", dataTableOutput("table"))
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
      addCircleMarkers(~longitude, ~latitude, popup = data$species, radius = 2, weight = 5, opacity = 0.5, fill= TRUE, fillOpacity = 0.2)
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
# Add summary plot counting species by occurrences. filter(n > 10) to limit swamping the chart. Note that the n value filter will often need adjustment for the dataset
  output$summary <- renderPlot({
    data %>% 
      filter(data$kingdom %in% input$kingdom_input) %>% 
      count(species) %>%
      dplyr::filter(n > 100) %>%
      ggplot(aes(x = reorder(species, n), y = n, fill = species)) +
      geom_bar(stat="identity", show.legend = FALSE) +
      coord_flip() + labs(x = "species", y = "occurrences") 
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("kenya_slim", ".csv", sep="")
    },
    content = function(file) {
      file.copy("kenya_slim.csv", file)
    }, contentType = "text/csv"
  )
}

shinyApp(ui = ui, server = server)
