library(shiny)
library(readr)
library(ggplot2)
library(rnaturalearth)
library(tidyverse)
# ... [Other libraries and existing code] ...

ui <- fluidPage(
  
  # UI for time series chart
  plotOutput("nutrition")
)

library(readxl)
library(corrplot)
library(GGally)
library(factoextra)

nutrition_data <- read_excel("pffy-data-mcdonalds-nutrition-facts.xlsx")
nutrition_data <- nutrition_data %>%
  mutate(ITEM = str_replace_all(ITEM, "\\s*\\((?!Small|Medium|Large).*\\)|\\s*\\d+\\s*(fl oz|oz|ml|g|lb|kg)\\b", ""))
numdata = nutrition_data %>% select_if(is.numeric)
corr_matrix <- cor(numdata, use = "complete.obs")  

# Step 4: Create the Correlation Plot
corrplot(corr_matrix, method = "color")  # Using corrplot
ggcorr(corr_matrix, hc.order = TRUE, type = "lower")  

nutrition_data_scaled <- scale(numdata)

# Step 3: Choosing the Number of Clusters (K)
set.seed(123)  # for reproducibility
wss <- sapply(1:10, function(k){kmeans(nutrition_data_scaled, k, nstart = 10)$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
kmeans_result <- kmeans(nutrition_data_scaled, centers = 5, nstart = 10)

pca_result <- prcomp(nutrition_data_scaled, center = TRUE, scale. = TRUE)

# Extract the first two principal components
pca_data <- as.data.frame(pca_result$x[, 1:2])
pca_data$cluster <- kmeans_result$cluster

pca_data$ITEM <- nutrition_data$ITEM

# Step 2: Plot the Clusters with Item Labels
ggplot(pca_data, aes(x = PC1, y = PC2, label = ITEM, color = as.factor(cluster))) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = ITEM), vjust = 2, hjust = 0.5, check_overlap = TRUE, size = 3) +
  labs(color = "Cluster", title = "K-means Clustering with PCA", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

nutrition_data$cluster <- kmeans_result$cluster

# Create the scatter plot
ggplot(nutrition_data, aes(x = CAL, y = PRO, color = as.factor(cluster))) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = ITEM), vjust = 1.5, hjust = 0.5, check_overlap = TRUE, size = 3) +
  labs(color = "Cluster", title = "Calories vs Protein: K-means Clustering", x = "Calories", y = "Protein") +
  theme_minimal() +
  xlim(c(0, 1300)) +  # Set x-axis limits
  ylim(c(0, 55))  # Set y-axis limits


server <- function(input, output, session) {
  
  # Render the time series chart
  output$nutrition <- renderPlot({
    ggplot(nutrition_data, aes(x = CAL, y = PRO, color = as.factor(cluster))) +
      geom_point(alpha = 0.7) +
      geom_text(aes(label = ITEM), vjust = 1.5, hjust = 0.5, check_overlap = TRUE, size = 3) +
      labs(color = "Cluster", title = "Calories vs Protein: K-means Clustering", x = "Calories", y = "Protein") +
      theme_minimal() +
      xlim(c(0, 1300)) +  # Set x-axis limits
      ylim(c(0, 55))  # Set y-axis limits
  })
  
  # ... [Any additional server logic] ...
}

# ... [Rest of the existing code] ...
shinyApp(ui, server)