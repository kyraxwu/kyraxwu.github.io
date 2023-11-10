# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Big Mac Price Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select a Year:", min = 2000, max = 2023, value = 2023),
      br(),
      downloadButton("downloadData", "Download Filtered Data")
    ),
    mainPanel(
      plotOutput("pricePlot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Load the CSV data
  bigmac_data <- read.csv("/Users/kyrawu/kyraxwu.github.io/McDonalds/DataSet/BigmacPrice.csv", stringsAsFactors = FALSE)
  
  # Convert the 'date' column to a Date object
  bigmac_data$date <- as.Date(bigmac_data$date)
  
  # Define a reactive expression for filtered data
  filtered_data <- reactive({
    bigmac_data %>%
      filter(year(date) == input$year)
  })
  
  # Create a plot of Big Mac prices
  output$pricePlot <- renderPlot({
    filtered_data() %>%
      ggplot(aes(x = name, y = dollar_price, fill = currency_code)) +
      geom_bar(stat = "identity") +
      labs(title = "Big Mac Prices by Country", x = "Country", y = "Price in USD") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Create a download button for filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("bigmac_prices_", input$year, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file)
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)