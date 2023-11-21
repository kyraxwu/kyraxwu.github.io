library(shiny)
library(plotly)
library(readxl)
library(tidyverse)

nutrition_data <- read_excel("pffy-data-mcdonalds-nutrition-facts.xlsx") %>% mutate(CATEGORY = tolower(CATEGORY))
numdata = nutrition_data %>% select_if(is.numeric)

ui <- fluidPage(
  titlePanel("McNutrition Navigator"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("variables", "Select Variables:", choices = colnames(numdata), selected = colnames(numdata))
    ),
    mainPanel(
      plotlyOutput("corrPlot")
    )
  )
)

server <- function(input, output) {
  output$corrPlot <- renderPlotly({
    data <- numdata[, input$variables]
    corr_matrix <- cor(data, use = "complete.obs")
    
    plot_ly(x = colnames(corr_matrix), y = colnames(corr_matrix), z = corr_matrix, type = "heatmap", colors = colorRamp(c("red", "yellow")), zmin = -1, zmax = 1) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = ""))
  })
}

shinyApp(ui = ui, server = server)
