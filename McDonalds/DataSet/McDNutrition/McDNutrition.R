library(shiny)
library(ggplot2)
library(tidyverse)
library(readxl)
library(corrplot)
library(GGally)
library(cluster)
library(factoextra)
library(tidytext) 
library(plotly)

nutrition_data <- read_excel("pffy-data-mcdonalds-nutrition-facts.xlsx") %>% mutate(CATEGORY = tolower(CATEGORY))
menu_items  = nutrition_data$ITEM
words <- menu_items %>%
  as_tibble() %>%
  unnest_tokens(word, value) %>% filter(!str_detect(word, "\\d+")) 

word_freq <- words %>%
  count(word, sort = TRUE)%>%filter(n>=8, nchar(word)>=3)

tags <- word_freq$word

add_tags <- function(item, category) {
  matching_tags <- tags[sapply(tags, function(t) grepl(t, item, ignore.case = TRUE))]
  new_category <- paste(c(category, matching_tags), collapse = ", ")
  return(new_category)
}

nutrition_data <- nutrition_data %>%
  rowwise() %>%
  mutate(CATEGORY = add_tags(ITEM, CATEGORY))

options <- setdiff(names(nutrition_data), c("ITEM", "CATEGORY"))

ui <- fluidPage(
  titlePanel("McDonald's Nutrition Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("xaxis", "Select Your First Variable (X-axis):", choices = options),
      selectInput("yaxis", "Select Your Second Variable (Y-axis):", choices = options),
      selectInput("color", "Select Third Variable (Colour):", choices = c("None", options)),
      selectizeInput("tags", "Select Tags:", choices = tags, multiple = TRUE),
      radioButtons("tagLogic", "Tag Filter Logic:", choices = c("OR" = "or", "AND" = "and"))
    ),
    mainPanel(
      plotlyOutput("nutrition")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- nutrition_data
    if (length(input$tags) > 0) {
      if (input$tagLogic == "and") {
        for (tag in input$tags) {
          data <- data %>% filter(str_detect(CATEGORY, tag))
        }
      } else {
        data <- data %>% filter(str_detect(CATEGORY, paste(input$tags, collapse = "|")))
      }
    }
    data
  })
  
  output$nutrition <- renderPlotly({
    req(input$xaxis, input$yaxis)
    
    p <- ggplot(filtered_data(), aes_string(x = input$xaxis, y = input$yaxis)) +
      geom_point(alpha = 0.7, aes(text = paste("<b style='font-size:20px;'>", ITEM, "</b><br>",
                                               "X-Axis:", input$xaxis, ": ", get(input$xaxis), "<br>",
                                               "Y-Axis:",input$yaxis, ": ", get(input$yaxis), "<br>",
                                               "<br>",
                                               "Calories: ", CAL, "<br>",
                                               "Total Fat (g): ", FAT, "<br>",
                                               "Trans Fat (g): ", TFAT, "<br>",
                                               "Saturated Fat (g): ", SFAT, "<br>",
                                               "Cholesterol (mg): ", CHOL, "<br>",
                                               "Sodium (mg): ", SALT, "<br>",
                                               "Carbohydrates (g): ", CARB, "<br>",
                                               "Dietary Fiber (g): ", FBR, "<br>",
                                               "Sugars (g): ", SGR, "<br>",
                                               "Protein (g): ", PRO))) +
      labs(title = "Nutrition Data Scatter Plot", x = input$xaxis, y = input$yaxis) +
      theme_minimal()
    
    if (input$color != "None") {
      p <- p + aes_string(color = input$color) +
        scale_color_gradient(low = "#FFC30B", high = "red")
    }
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)
