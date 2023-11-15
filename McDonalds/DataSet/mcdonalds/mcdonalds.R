library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(sf)
library(jsonlite)

# Define UI
ui <- fluidPage(
  # Include custom CSS
  tags$head(
    tags$style(HTML("
      body {background-color: #f7f7f7;} 
      .well {background-color: #ffffff; border-radius: 8px; box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);} 
      .shiny-input-container {padding: 10px;} 
      #viewType label {font-weight: bold;} 
      .leaflet-container {background: #f9f9f9; border-radius: 8px;} 
      .btn {background-color: #007bff; color: white; border-radius: 4px;} 
      .btn:hover {background-color: #0056b3;}
    "))
  ),
  
  # App Content
  fluidRow(
    column(12, 
           div(leafletOutput("map"), style = "border-radius: 8px; overflow: hidden;")
    )
  ),
  fluidRow(
    column(12, 
           wellPanel(
             radioButtons("viewType", "Choose View:",
                          choices = c("McDonald's Outlets per Million Residents" = "outlets_per_mil",
                                      "GDP per Capita" = "gdp_per_capita"))
           )
    )
  )
)


# Define server
server <- function(input, output, session) {
  
  # Read the outlets data
  df <- st_read("outlets.csv") 
  df <- df %>%
    mutate(
      across(c(2, 3, 5, 6, 9, 10), as.numeric), gdp_per_capita = gdp_md_est/pop_est*1000000
    )  
  geometries <- st_as_sfc(df$geometry, crs = 4326) # Ensure the CRS is correct for your data
  # Combine the geometries with the rest of the data frame
  df_sf <- st_sf(df, geometry = geometries)
  # Create Leaflet map with colored country borders
  
  observe({
    # Decide which data to display based on the input
    if(input$viewType == "outlets_per_mil"){
      colorData <- df$outlets_per_mil
    } else {
      colorData <- df$gdp_per_capita
    }
    breaks <- quantile(colorData, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
    palette <- colorBin("YlOrRd", colorData, bins = breaks, na.color = "#808080")
    output$map <- renderLeaflet({
      leaflet(df_sf) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~palette(colorData), #fill according to selected data
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
        addLegend("bottomright", 
                  pal = palette, 
                  values = ~colorData,
                  title = ifelse(input$viewType == "outlets_per_mil", "McDonald's Outlets per Million Residents", "GDP per Capita"),
                  labels = round(breaks),
                  opacity = 1,) %>%
        setView(lat = 0, lng = 0, zoom = 2)
    })
  })
}

# Run the application
shinyApp(ui, server)
