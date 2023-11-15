library(shiny)
library(readr)
library(ggplot2)
library(rnaturalearth)
library(tidyverse)
# ... [Other libraries and existing code] ...

ui <- fluidPage(
  # Include custom CSS
  tags$head(
    tags$style(HTML("
      body {background-color: #f7f7f7;} 
      .well {background-color: #ffffff; border-radius: 8px; box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);} 
      .shiny-input-container {padding: 10px;} 
      .selectize-input {border-radius: 4px; border: 1px solid #ced4da; height: calc(2.25rem + 2px);} 
      .selectize-dropdown {border-radius: 4px; border: 1px solid #ced4da;}
      .btn {background-color: #007bff; color: white; border-radius: 4px;} 
      .btn:hover {background-color: #0056b3;}
      #bigMacTimeSeries {border-radius: 8px; overflow: hidden; background-color: #f9f9f9;}
    "))
  ),
  
  # App Content
  fluidRow(
    column(12, 
           div(plotOutput("bigMacTimeSeries"), style = "border-radius: 8px; overflow: hidden;")
    )
  ),
  fluidRow(
    column(12, 
           wellPanel(
             selectInput("grouping", "Select Grouping:",
                         choices = c("Continent" = "continent",
                                     "Subregion" = "subregion",
                                     "Economy" = "economy",
                                     "Income Group" = "income_grp"))
           ),
           checkboxInput("showAnnotations", "Show Annotations", TRUE)
           
    )
  )
)

server <- function(input, output, session) {
  # Read the Big Mac Index data
  bigMacData <- read_csv("BigmacPrice.csv") %>%
    mutate(name = case_when(
      name == "Hong Kong" ~ "Hong Kong S.A.R.",
      name == "Britain" ~ "United Kingdom",
      name == "UAE" ~ "United Arab Emirates",
      name == "United States" ~ "United States of America",
      TRUE ~ name
    ))
  world_countries <- ne_countries(scale = "medium", returnclass = "sf")
  bigMacData <- bigMacData %>%
    left_join(world_countries, by = c("name" = "admin")) %>% 
    select("name","date", "dollar_price", "continent", "subregion",
           "pop_est", "gdp_md_est", "economy", "income_grp") 
  
  #avg by continent
  averagePriceByContinent <- bigMacData %>%
    group_by(continent, date) %>%
    summarize(average_dollar_price = mean(dollar_price, na.rm = TRUE)) %>%
    ungroup()  
  #avg by subregion
  averagePriceBysubregion <- bigMacData %>%
    group_by(subregion, date) %>%
    summarize(average_dollar_price = mean(dollar_price, na.rm = TRUE)) %>%
    ungroup()  
  #avg by economy
  averagePriceByeconomy <- bigMacData %>%
    group_by(economy, date) %>%
    summarize(average_dollar_price = mean(dollar_price, na.rm = TRUE)) %>%
    ungroup()  
  #avg by economy
  averagePriceByincome <- bigMacData %>%
    group_by(income_grp, date) %>%
    summarize(average_dollar_price = mean(dollar_price, na.rm = TRUE)) %>%
    ungroup()  
  
  selectedData <- reactive({
    switch(input$grouping,
           "continent" = averagePriceByContinent,
           "subregion" = averagePriceBysubregion,
           "economy" = averagePriceByeconomy,
           "income_grp" = averagePriceByincome)
  })
  # Render the time series chart
  output$bigMacTimeSeries <- renderPlot({
    
    # Start with the basic ggplot object
    p <- ggplot(selectedData(), aes_string(x = "date", y = "average_dollar_price", group = input$grouping, color = input$grouping)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Big Mac Index Over Time", x = "Date", y = "Price (Local Currency)")
    
    # Conditionally add annotations if the checkbox is checked
    if (input$showAnnotations) {
      p <- p + 
        geom_vline(xintercept = as.Date("2003-09-02"), linetype = "dashed", color = "gold") +
        geom_vline(xintercept = as.Date("2004-07-08"), linetype = "dashed", color = "red") +
        geom_rect(aes(xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.002) +
        geom_rect(aes(xmin = as.Date("2020-03-01"), xmax = as.Date("2020-06-30"), ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.002) +
        annotate("text", x = as.Date("2003-09-02"), y = Inf, label = "I'm Lovin' It launch", vjust = 1.5, color = "gold", fontface = "bold") +
        annotate("text", x = as.Date("2004-07-08"), y = Inf, label = "Super Size Me Documentary", vjust = 2, color = "red", fontface = "bold") +
        annotate("text", x = as.Date("2007-12-01"), y = Inf, label = "Global Financial Crisis", vjust = 2.75, color = "red", fontface = "bold") +
        annotate("text", x = as.Date("2020-03-01"), y = Inf, label = "COVID Lockdown", vjust = 3.75, color = "red", fontface = "bold")
    }
    
    # Return the plot
    p
  })
}

shinyApp(ui, server)