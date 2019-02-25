
# Shiny Visualisation -----------------------------------------------------
library(shiny)
library(tidyverse)
library(readr)
library(leaflet)
viz_data <- read_csv("viz_data.csv")
viz_data$year <- as.factor(viz_data$year)
viz_data$form <- as.factor(viz_data$form)

# UI Code -----------------------------------------------------------------

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("month", "Month", 1, 12,
                            value = 1, step = 1, sep = ""
                ),
                selectInput("year", "Year", choices = levels(viz_data$year), multiple = TRUE
                ),
                selectInput("form", "Type of Form",choices = levels(viz_data$form), multiple = TRUE),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    viz_data %>% 
      filter(month %in% c(input$month), year %in% c(input$year), form %in% c(input$form))
  })
  
  # Color Function
  getColor <- function(data) {
    sapply(data$time_mean, function(time_mean){
      if (time_mean <= 7){
        "green"
      } else if (time_mean <= 14) {
        "orange"
      } else {
        "red"
      } })
  }
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  icons <- reactive({
    awesomeIcons(
      icon = 'ios-time',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(filteredData())
      )
  })
    
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(viz_data) %>% addTiles() %>%
      fitBounds(~min(LNG), ~min(LAT), ~max(LNG), ~max(LAT))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    icon <- icons()
    leafletProxy("map", data = filteredData()) %>%
      addAwesomeMarkers(lng = ~LNG, lat = ~LAT, icon = icon, label = ~paste("Min: ",time_min,"Max:",time_max))
  })
}

shinyApp(ui, server)
