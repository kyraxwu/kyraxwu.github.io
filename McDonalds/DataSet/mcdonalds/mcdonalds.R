library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(sf)
library(jsonlite)

# Define UI
ui <- fluidPage(
  leafletOutput("map")
)

# Define server
server <- function(input, output, session) {
  
  # Read the outlets data
  df <- st_read("outlets.csv") 
  df <- df %>%
    mutate(
      across(c(2, 3, 5, 6, 9, 10), as.numeric)
    )  # Create a color palette function
  palette <- colorQuantile("YlOrRd", df$outlets_per_mil)
  geometries <- st_as_sfc(df$geometry, crs = 4326) # Ensure the CRS is correct for your data
  
  # Combine the geometries with the rest of the data frame
  df_sf <- st_sf(df, geometry = geometries)
  # Create Leaflet map with colored country borders
  output$map <- renderLeaflet({
    leaflet(df_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~palette(outlets_per_mil),
        color = "#000000", # Border color
        fillOpacity = 0.7,
        weight = 1,
        dashArray = "3",
        smoothFactor = 0.5,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste(
          "Country: ", name, "<br/>",
          "Stores per Million: ", round(outlets_per_mil, 2), "<br/>",
          "Number of Outlets: ", num_outlets, "<br/>",
          "First Store Opened On: ", `First.Opening`,"<br/>",
          "GDP (Billion USD): ", round(gdp_md_est/1000, 2)
          
        )%>%
          lapply(htmltools::HTML)
      ) %>%
      setView(lat = 0, lng = 0, zoom = 2)
  })
}
?addPolygons()

# Run the application
shinyApp(ui, server)
