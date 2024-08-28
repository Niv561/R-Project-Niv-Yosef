install.packages("tidyverse")
install.packages("caret")
install.packages("corrplot")
install.packages("GGally")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("rsample")
install.packages("AmesHousing")
install.packages("DT")
install.packages("flexdashboard")
install.packages("plotly")
install.packages("ggthemes")
install.packages("randomforest")

library(tidyverse)
library(caret)
library(corrplot)
library(GGally)
library(ggplot2)
library(AmesHousing)
library(scales)
library(corrplot)
library(dplyr)
library(DT)
library(flexdashboard)
library(randomforest)

ames_data <- make_ames()

# Summary statistics by sale price
summary_stats <- ames_data %>%
  summarise(
    Min_Price = min(Sale_Price),
    Max_Price = max(Sale_Price),
    Median_Price = median(Sale_Price),
    Mean_Price = mean(Sale_Price),
    Total_Houses = n())

# General data view
view(summary_stats)
view(ames_data)
colnames(ames_data)
summary(ames_data)

# Histogram of SalePrice with formatted y-axis
ggplot(ames_data, aes(x = Sale_Price)) +
  geom_histogram(binwidth = 10000, fill = "red") +
  labs(title = "Distribution of Sale Prices", x = "Sale Price", y = "Count") +
  theme_minimal() +
  scale_x_continuous(labels = dollar_format())

#PLOT1
# Bar plot of OverallQual (no need to format prices here)
ggplot(ames_data, aes(x = as.factor(Overall_Qual))) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of Overall Quality", x = "Overall Quality", y = "Count") +
  theme_minimal()

# Boxplot of SalePrice by OverallQual with formatted y-axis
ggplot(ames_data, aes(x = as.factor(Overall_Qual), y = Sale_Price)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Sale Price by Overall Quality", x = "Overall Quality", y = "Sale Price") +
  theme_minimal() +
  scale_y_continuous(labels = dollar_format())

#PLOT2
# Create a new variable for total square footage
ames_data <- ames_data %>%
  mutate(Total_SF = Gr_Liv_Area + Total_Bsmt_SF)

# Create bins for total square footage
ames_data <- ames_data %>%
  mutate(SF_Category = case_when(
    Total_SF < 1500 ~ "Small",
    Total_SF >= 1500 & Total_SF < 2500 ~ "Medium",
    Total_SF >= 2500 ~ "Large"
  ))

# Create a boxplot of SalePrice by SF_Category
ggplot(ames_data, aes(x = SF_Category, y = Sale_Price)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Sale Price by Total Square Footage Category", x = "Square Footage Category", y = "Sale Price") +
  theme_minimal() +
  scale_y_continuous(labels = dollar_format())

#PLOT3
# Select only numeric columns
numeric_data <- select_if(ames_data, is.numeric)

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Extract correlations with SalePrice
saleprice_correlation <- correlation_matrix["Sale_Price",]
saleprice_correlation <- saleprice_correlation[!names(saleprice_correlation) %in% "Sale_Price"]

# Convert the correlation data to a data frame for ggplot2
correlation_df <- data.frame(
  Feature = names(saleprice_correlation),
  Correlation = saleprice_correlation
)

# Create the heatmap with correlation values
ggplot(correlation_df, aes(x = reorder(Feature, Correlation), y = Correlation)) +
  geom_tile(aes(fill = Correlation), color = "white") +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  labs(title = "Correlation of Sale Price with Other Features", 
       x = "Features", y = "Correlation with Sale Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





ames_rg <- ames_data %>%
  mutate_if(is.character, as.factor)

set.seed(123)
trainIndex <- createDataPartition(ames_data$Sale_Price, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
housing_train <- ames_rg[trainIndex,]
housing_test <- ames_rg[-trainIndex,]

lm_model <- lm(Sale_Price ~ Year_Built, data = housing_train)
summary(lm_model)

ggplot(housing_train, aes(x = Year_Built, y = Sale_Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(limits = c(1875, max(housing_train$Year_Built))) +
  scale_y_continuous(limits = c(0, max(housing_train$Sale_Price))) +
  labs(title = "Linear Regression: Sale Price vs. Year Built",
       x = "Year Built",
       y = "Price") +
theme_gdocs() + 
  scale_y_continuous(labels = dollar_format())



#New Linear reg model 
# Preprocess the Data
ames_rg <- ames_data %>%
  mutate_if(is.character, as.factor) %>%
  filter(!is.na(Sale_Price))

# Split the Data
set.seed(123)
trainIndex <- createDataPartition(ames_rg$Sale_Price, p = 0.8, list = FALSE, times = 1)
ames_rg_train <- ames_rg[trainIndex,]
ames_rg_test <- ames_rg[-trainIndex,]

# Linear Regression Analysis
lm_model <- lm(Sale_Price ~ Gr_Liv_Area + Overall_Qual + Year_Built + TotRms_AbvGrd + Neighborhood, data = ames_rg_train)
summary(lm_model)

# Combined Plot: Sale Price vs. Gr Liv Area colored by Neighborhood
ggplot(ames_rg_train, aes(x = Gr_Liv_Area, y = Sale_Price, color = Neighborhood)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Neighborhood)) +
  labs(title = "Sale Price vs. Gr Liv Area by Neighborhood",
       x = "Gr Liv Area (Sq Ft)",
       y = "Sale Price",
       color = "Neighborhood") +
  theme_gdocs() + 
  scale_y_continuous(labels = dollar_format())


#ML 
library(cluster)

# Preprocess the Data
ames_rg1 <- ames_rg %>%
  mutate_if(is.character, as.factor) %>%
  filter(!is.na(Sale_Price))

# Selecting relevant features for clustering
features <- ames_rg1 %>%
  select(Sale_Price, Gr_Liv_Area, Overall_Qual, Year_Built, TotRms_AbvGrd, Neighborhood)

# Converting Neighborhood to numerical values for clustering
features$Neighborhood <- as.numeric(features$Neighborhood)

# Scaling the features
features <- features %>%
  mutate(across(everything(), as.numeric))
scaled_features <- scale(features)

# Perform k-means clustering with 3 clusters
set.seed(123)
kmeans_model <- kmeans(scaled_features, centers = 3, nstart = 25)

# Add cluster assignment to the original data
ames_rg1$Cluster <- as.factor(kmeans_model$cluster)

# Visualization of Clusters
ggplot(ames_rg1, aes(x = Gr_Liv_Area, y = Sale_Price, color = Cluster)) +
  geom_point(alpha = 0.7) +
  labs(title = "Clusters of Sale Price vs. Gr Liv Area",
       x = "Gr Liv Area (Sq Ft)",
       y = "Sale Price",
       color = "Cluster") +
  theme_gdocs() + 
  scale_y_continuous(labels = dollar_format())