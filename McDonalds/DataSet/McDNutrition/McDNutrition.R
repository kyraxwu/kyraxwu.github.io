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
#nutrition_data <- nutrition_data %>%
#mutate(ITEM = str_replace_all(ITEM, "\\s*\\((?!Small|Medium|Large).*\\)|\\s*\\d+\\s*(fl oz|oz|ml|g|lb|kg)\\b", ""))
numdata = nutrition_data %>% select_if(is.numeric)
corr_matrix <- cor(numdata, use = "complete.obs")  # 'use' parameter handles missing values

# Step 4: Create the Correlation Plot
corrplot(corr_matrix, method = "color")  # Using corrplot
ggcorr(corr_matrix, hc.order = TRUE, type = "lower")  # Using GGally for a different style

#nutrition_data_scaled <- scale(numdata)
#lets get tags that we can use to filter search with:
# Tokenize the menu items into words
menu_items  = nutrition_data$ITEM
words <- menu_items %>%
  as_tibble() %>%
  unnest_tokens(word, value) %>% filter(!str_detect(word, "\\d+")) 

# Count word frequencies
word_freq <- words %>%
  count(word, sort = TRUE)%>%filter(n>=8, nchar(word)>=3)

tags <- word_freq$word

# Function to add tags to CATEGORY
add_tags <- function(item, category) {
  # Find tags that are in the item
  matching_tags <- tags[sapply(tags, function(t) grepl(t, item, ignore.case = TRUE))]
  
  # Combine existing category with new tags, separated by commas
  new_category <- paste(c(category, matching_tags), collapse = ", ")
  
  # Return the updated category
  return(new_category)
}

# Apply the function to each row in nutrition_data
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
      plotlyOutput("nutrition"),  # Changed from plotOutput to plotlyOutput
      plotOutput("corrPlot")
    )
  )
)


server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- nutrition_data
    if (length(input$tags) > 0) {
      if (input$tagLogic == "and") {
        # Apply AND logic
        for (tag in input$tags) {
          data <- data %>% filter(str_detect(CATEGORY, tag))
        }
      } else {
        # Apply OR logic
        data <- data %>% filter(str_detect(CATEGORY, paste(input$tags, collapse = "|")))
      }
    }
    data
  })
  
  output$nutrition <- renderPlotly({
    req(input$xaxis, input$yaxis)  # Ensure these inputs are selected
    
    # Create ggplot object
    p <- ggplot(filtered_data(), aes_string(x = input$xaxis, y = input$yaxis, text = "ITEM")) +
      geom_point(alpha = 0.7) +
      labs(title = "Nutrition Data Scatter Plot", x = input$xaxis, y = input$yaxis) +
      theme_minimal()
    
    # If color is selected, add it to the plot
    if (input$color != "None") {
      p <- p + aes_string(color = input$color)
    }
    
    # Convert to Plotly and add tooltip
    ggplotly(p, tooltip = "text")  # Tooltip shows the value of "ITEM"
  })
  
  output$corrPlot <- renderPlot({
    corrplot(corr_matrix, method = "color")
  })
}


shinyApp(ui, server)