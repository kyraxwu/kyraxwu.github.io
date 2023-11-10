library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(sf)

# Define UI
ui <- fluidPage(
  leafletOutput("map")
)

# Define server
server <- function(input, output, session) {
  
  # Read the data
  df <- read_csv("outlets.csv")
  
  # Convert to an sf object and calculate centroids
  outlets_sf <- df %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    group_by(Country) %>%
    summarise(geometry = st_union(geometry)) %>%
    st_centroid() %>%
    left_join(df, by = "Country") # Join additional info
  
  # Define the color palette
  mcdonalds_red <- "#DA291C"
  mcdonalds_yellow <- "#FFC72C"
  
  # Create Leaflet map
  output$map <- renderLeaflet({
    leaflet(outlets_sf, options = leafletOptions(dragging = TRUE)) %>%
      addProviderTiles("CartoDB.Voyager") %>%
      setView(lat = 0, lng = 0, zoom = 2) %>%
      addCircleMarkers(
        lng = ~st_coordinates(geometry)[, 1], 
        lat = ~st_coordinates(geometry)[, 2],
        radius = ~sqrt(outlets_per_mil) *1.5, # Adjust the scaling factor as needed
        fillColor = mcdonalds_red, 
        fillOpacity = 0.8, 
        color = mcdonalds_yellow, 
        weight = 1,
        stroke = TRUE,
        label = ~paste(
          "Country: ", Country, "<br/>",
          "Stores per Million: ", round(outlets_per_mil, 2), "<br/>",
          "Number of Outlets: ", num_outlets, "<br/>",
          "First Opening: ", `First Opening`
        )%>%
          lapply(htmltools::HTML)
      )
  })
}

# Run the application
shinyApp(ui, server)
