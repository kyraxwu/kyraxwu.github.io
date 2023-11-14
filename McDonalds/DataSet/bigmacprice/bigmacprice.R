library(shiny)
library(readr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)

ui <- fluidPage(
  # ... [Existing UI elements] ...
  
  # UI for time series chart
  plotOutput("bigMacTimeSeries")
)

server <- function(input, output, session) {
  
  # ... [Existing server logic] ...
  
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
  
  ggplot(averagePriceByContinent, aes(x = date, y = average_dollar_price, group = continent, color = continent)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Big Mac Index Over Time", x = "Date", y = "Price (Local Currency)")
  
  #avg by subregion
  averagePriceBysubregion <- bigMacData %>%
    group_by(subregion, date) %>%
    summarize(average_dollar_price = mean(dollar_price, na.rm = TRUE)) %>%
    ungroup()  
  
  ggplot(averagePriceBysubregion, aes(x = date, y = average_dollar_price, group = subregion, color = subregion)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Big Mac Index Over Time", x = "Date", y = "Price (Local Currency)")
  
  #avg by economy
  averagePriceByeconomy <- bigMacData %>%
    group_by(economy, date) %>%
    summarize(average_dollar_price = mean(dollar_price, na.rm = TRUE)) %>%
    ungroup()  
  
  ggplot(averagePriceByeconomy, aes(x = date, y = average_dollar_price, group = economy, color = economy)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Big Mac Index Over Time", x = "Date", y = "Price (Local Currency)")
  
  #avg by economy
  averagePriceByincome <- bigMacData %>%
    group_by(income_grp, date) %>%
    summarize(average_dollar_price = mean(dollar_price, na.rm = TRUE)) %>%
    ungroup()  
  
  ggplot(averagePriceByincome, aes(x = date, y = average_dollar_price, group = income_grp, color = income_grp)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Big Mac Index Over Time", x = "Date", y = "Price (Local Currency)")

  
  # Render the time series chart
  output$bigMacTimeSeries <- renderPlot({
    ggplot(averagePriceByincome, aes(x = date, y = average_dollar_price, group = income_grp, color = income_grp)) +
      geom_line() +
      # Other vertical lines
      geom_vline(xintercept = as.Date("2003-09-02"), linetype = "dashed", color = "green") +
      geom_vline(xintercept = as.Date("2004-07-08"), linetype = "dashed", color = "red") +
      # Translucent rectangles for events
      geom_rect(aes(xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.005) +
      geom_rect(aes(xmin = as.Date("2020-03-01"), xmax = as.Date("2020-06-30"), ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.005) +
      # Captions
      annotate("text", x = as.Date("2003-09-02"), y = Inf, label = "I'm Lovin' It launch", vjust = 1.5, color = "green") +
      annotate("text", x = as.Date("2004-07-08"), y = Inf, label = "Super Size Me", vjust = 2, color = "red") +
      annotate("text", x = as.Date("2007-12-01"), y = Inf, label = "Global Financial Crisis", vjust = 2.75, color = "red") +
      annotate("text", x = as.Date("2020-03-01"), y = Inf, label = "COVID Lockdown", vjust = 3.75, color = "red") +
      theme_minimal() +
      labs(title = "Big Mac Index Over Time", x = "Date", y = "Price (Local Currency)")
  })
  
  # ... [Any additional server logic] ...
}

# ... [Rest of the existing code] ...
shinyApp(ui, server)