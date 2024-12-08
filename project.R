library(MASS)
library(tree)
library(ggplot2)
library(car)
library(tree)
library(DAAG)

library(dplyr)
library(readr)
library(tidyr)
library(corrplot)
#-----------------------------------------------------------------------
#EDA (Exploratory Data Analysis)
# Load Libraries


# Load Data
df <- read_csv("/Users/apple/Desktop/Study/semester4/DataAnalytics_ICT513/project/birddataset.csv")
head(df)

# Composition
# Number of rows and columns
cat("The number of rows:", nrow(df), "\nThe number of columns:", ncol(df), "\n")

# Data Structure Information
str(df)

# Round the percentage to 2 decimal places
class_count <- round(prop.table(table(df$SPECIES)) * 100, 2)
class_count <- sort(class_count, decreasing = TRUE)
class_count

# Find the value counts in percentage for species == "S1"
df_s1 <- df %>% filter(SPECIES == "S1")
round(prop.table(table(df_s1$HABITAT)) * 100, 2)

# Group by Species, Habitat, and Height_max to calculate percentage
result <- df %>%
  drop_na(Height_max) %>%
  group_by(Height_max, HABITAT, SPECIES) %>%
  summarise(count = n()) %>%
  mutate(Percentage = round(count / nrow(df) * 100, 2)) %>%
  arrange(desc(Percentage))
print(result)

# Summary of SPECIES
summary(df$SPECIES)

# Distribution
# Summary statistics for Height_max
summary(df$Height_max)

# Drop NA and summarize
filtered_data <- df %>% drop_na(Height_max)
summary(filtered_data$Height_max)

# Plot distributions of numerical variables
ggplot(filtered_data, aes(x = Height_max)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Maximum Flight Height", x = "Height_max")

# Plot box plot of Height_max
ggplot(filtered_data, aes(y = Height_max)) +
  geom_boxplot(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Box Plot of Maximum Flight Height", y = "Height_max")

# Density plot for Height_max
ggplot(filtered_data, aes(x = Height_max)) +
  geom_density(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Maximum Flight Height", x = "Height_max", y = "Density")

# Calculate the IQR
Q1 <- quantile(filtered_data$Height_max, 0.25, na.rm = TRUE)
Q3 <- quantile(filtered_data$Height_max, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

print(IQR)
# Define the lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
print(lower_bound)
print(upper_bound)

# Remove outliers
df_no_outliers <- filtered_data %>%
  filter(Height_max >= lower_bound & Height_max <= upper_bound)

print(df_no_outliers)

# Impute outliers with the median value
median_value <- median(df$Height_max, na.rm = TRUE)

df$Height_max <- ifelse(df$Height_max < lower_bound | df$Height_max > upper_bound,
                        median_value,
                        df$Height_max)
#After box plot
# Plot box plot of Height_max
ggplot(df_no_outliers, aes(y = Height_max)) +
  geom_boxplot(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Box Plot of Maximum Flight Height", y = "Height_max")

# After Removing Outliers Plot distributions of numerical variables
ggplot(df_no_outliers, aes(x = Height_max)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Maximum Flight Height", x = "Height_max")

# After Fill Outliers Density plot for Height_max
ggplot(df_no_outliers, aes(x = Height_max)) +
  geom_density(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Maximum Flight Height", x = "Height_max", y = "Density")
######
ggplot(df, aes(x = Height_min)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Minimum Flight Height", x = "Height_min")

ggplot(df, aes(x = N_INDIVIDUALS)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Number of Individuals", x = "N_INDIVIDUALS")

# Check skewness for Height_max
library(e1071)
skewness(df_no_outliers$Height_max, na.rm = TRUE)

# Comparison
# Average Height_max by Species and Habitat
df_no_outliers %>%
  group_by(SPECIES, HABITAT) %>%
  summarise(average_height = mean(Height_max, na.rm = TRUE))

# Plot Average Max Flight Height by Habitat
avg_height_by_habitat <- df_no_outliers %>%
  group_by(HABITAT) %>%
  summarise(average_height = mean(Height_max, na.rm = TRUE))

ggplot(avg_height_by_habitat, aes(x = HABITAT, y = average_height)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Max Flight Height by Habitat", y = "Average Max Flight Height")

# Line chart for Average Max Flight Height by Habitat
ggplot(avg_height_by_habitat, aes(x = HABITAT, y = average_height, group = 1)) +
  geom_line(color = "skyblue", linewidth = 1) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Average Max Flight Height by Habitat", y = "Average Max Flight Height", x = "Habitat")


# Average Max Flight Height by Season
avg_height_by_season <- df_no_outliers %>%
  group_by(SEASON) %>%
  summarise(average_height = mean(Height_max, na.rm = TRUE))

ggplot(avg_height_by_season, aes(x = SEASON, y = average_height)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(title = "Average Max Flight Height by Season", y = "Average Max Flight Height")

# Relationship (Correlation)
# Correlation Matrix
corr_matrix <- df_no_outliers %>%
  select(Height_min, Height_max, N_INDIVIDUALS, Longitude, Latitude) %>%
  cor(use = "complete.obs")
print(corr_matrix)

# Load ggplot2 if not already loaded
library(ggplot2)

# Scatter plot for pairs of numerical variables
ggplot(df_no_outliers, aes(x = Height_min, y = Height_max)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Scatter Plot of Minimum vs Maximum Flight Height", x = "Height_min", y = "Height_max")

# Alternatively, to create scatter plots for all numerical variable pairs, you can use GGally package
# install.packages("GGally") # Uncomment to install GGally if not already installed
library(GGally)

# Scatter plot matrix for all numerical variables
numeric_df <- df_no_outliers %>% select(Height_min, Height_max, N_INDIVIDUALS, Longitude, Latitude)
ggpairs(numeric_df, title = "Scatter Plot Matrix for Numerical Variables")


# Plot Correlation Heatmap
corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black",
         title = "Correlation Matrix for Numerical Variables", mar = c(0,0,1,0))


#-------------------------------------------------------------------------------------------------------------------------



bird_data <- df_no_outliers
summary(bird_data)




#Question 1---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Classification tree
bird_data_clean <- subset(bird_data, !is.na(SPECIES) & 
                            !is.na(Height_max) &
                            !is.na(HABITAT))

bird_data_clean <- subset(bird_data_clean, Height_max >= 30)
# Convert SPECIES and HABITAT to factors if they aren't already
bird_data_clean$SPECIES <- as.factor(bird_data_clean$SPECIES)
bird_data_clean$HABITAT <- as.factor(bird_data_clean$HABITAT)


print(bird_data_clean)

set.seed(101)

# Split the cleaned data into training and testing sets
train_indices <- sample(1:nrow(bird_data_clean), 0.6 * nrow(bird_data_clean))
train_set <- bird_data_clean[train_indices, ]
test_set <- bird_data_clean[-train_indices, ]

train_set$SPECIES <- droplevels(train_set$SPECIES)
train_set$HABITAT <- droplevels(train_set$HABITAT)

length(unique(train_set$SPECIES))  # See if it's more than 32
length(unique(train_set$HABITAT))  # See if it's more than 32


unique_habitats <- unique(train_set$HABITAT)
print(unique_habitats)
unique_species <- unique(train_set$SPECIES)
print(unique_species)



# Now try fitting the tree model again
tree_model <- tree(SPECIES ~ log(Height_max) + HABITAT, data = train_set)
summary(tree_model)




# Plot the decision tree
plot(tree_model)
text(tree_model, pretty = 0)

# Cross-validate to find the best tree size
cv_model <- cv.tree(tree_model, FUN = prune.misclass)
plot(cv_model$size, cv_model$dev, type = 'b')
# Prune the tree to the optimal size
best_size <- cv_model$size[which.min(cv_model$dev)]
print(best_size)

# K Fold Cross Validation
# Cross-validate to find the best tree size with K folds method
cv_model <- cv.tree(tree_model, FUN = prune.tree, K = 10)  # K=10 for 10-fold cross-validation

# Define a function to calculate MSE for different tree sizes
calculate_mse <- function(tree_size, train_data, test_data) {
  pruned_tree <- prune.tree(tree_model, best = tree_size)
  
  # Check if the pruned tree has only a single node
  if (nrow(pruned_tree$frame) == 1) {
    return(c(NA, NA))
  }
  
  # Predict on training data and align factor levels
  train_predictions <- predict(pruned_tree, train_data, type = "class")
  train_predictions <- factor(train_predictions, levels = levels(train_data$SPECIES))
  train_mse <- mean(train_predictions != train_data$SPECIES)
  
  # Predict on test data and align factor levels
  test_predictions <- predict(pruned_tree, test_data, type = "class")
  test_predictions <- factor(test_predictions, levels = levels(test_data$SPECIES))
  test_mse <- mean(test_predictions != test_data$SPECIES)
  
  return(c(train_mse, test_mse))
}

# Apply the function to calculate MSE for each tree size
mse_values <- sapply(cv_model$size, calculate_mse, train_data = train_set, test_data = test_set)


# Convert MSE values to a data frame for ggplot
mse_df <- data.frame(
  TreeSize = rep(cv_model$size, each = 3),
  MSE = c(cv_model$dev / length(train_set$SPECIES), mse_values[1, ], mse_values[2, ]),
  Dataset = rep(c("Cross-Validation", "Training", "Test"), each = length(cv_model$size))
)

# Plot MSE for training, cross-validation, and test sets
ggplot(mse_df, aes(x = TreeSize, y = MSE, color = Dataset)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    x = "Tree Size",
    y = "Mean Squared Error",
    title = "Mean Squared Error by Tree Size",
    color = "Dataset"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Training" = "black", "Cross-Validation" = "blue", "Test" = "orange"))


#We find 6 fest throught K-fold method
pruned_tree <- prune.tree(tree_model, best = 6)
plot(pruned_tree)
text(pruned_tree)
summary(pruned_tree)

#Resolve label problem
# Replace `c(4, 12)` with the specific nodes you want to prune
pruned_tree_manual <- snip.tree(tree_model, nodes = c(4, 12)) 

# Plot the manually pruned tree with original category labels
plot(pruned_tree_manual)
text(pruned_tree_manual, pretty = 0)


# Predict on the test set
predictions <- predict(pruned_tree, newdata = test_set, type = "class")

# Evaluate model accuracy
confusion_matrix <- table(predictions, test_set$SPECIES)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Model Accuracy:", accuracy))

#----------------------------------------------------------------------------------------------------------------------------------
#Question 2 
#MLR (Multiple Linear Regression)
bird_data_clean <- subset(bird_data, !is.na(SPECIES) & 
                         !is.na(Height_max) &
                         !is.na(HABITAT))

bird_data_clean <- subset(bird_data_clean, Height_max >= 30)

#Visualse the relationship before applying the model
ggplot(bird_data_clean, aes(x = SPECIES, y = Height_max, color = HABITAT)) +
  geom_point() +
  labs(title = "Height_max by Species and Habitat", x = "Species", y = "Maximum Flight Height")

#Do the factorization because type of variables are vategorical
bird_data_clean$SPECIES <- as.factor(bird_data_clean$SPECIES)
bird_data_clean$HABITAT <- as.factor(bird_data_clean$HABITAT)

# Model with Height_max predicted by Species and Habitat
model <- lm(log(Height_max) ~ SPECIES + HABITAT, data = bird_data_clean)
summary(model)

#calculate 95% confidenc
confint(model)

#Diagnostic Check

#Normality of Residuals:
qqnorm(model$residuals)
qqline(model$residuals, col = "blue")


#Homoscedasticity (Equal Variance of Residuals):
plot(model$fitted.values, model$residuals, 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

#Variance Inflation Factor (VIF) for Multicollinearity:
vif(model)

#------------------------------------------------------------------------------------------------------------------------------
#Question 3
#Quesion3 with KNN Classification method----------------------------

# Load the "class" package to make use of the "knn.cv" function.
library(class)

# Assuming `bird_data` is the dataset with the provided columns:
# SPECIES, SITE_ID, Height_min, Height_max, Behaviour, WEATHER_CONDITIONS,
# N_INDIVIDUALS, SEASON, HABITAT, Longitude, Latitude

# Convert SITE_ID to a numeric or factor variable if it’s categorical.
bird_data$SITE_ID <- as.numeric(as.factor(bird_data$SITE_ID))  # Convert to numeric if it's categorical

# Standardize the relevant columns to ensure equal contribution in distance calculations
bird_data_scaled <- scale(bird_data[, c("Height_max", "SITE_ID", "Longitude", "Latitude")])

# Set the maximum number of nearest neighbors to consider
n_neighbors <- 10

# Initialize vectors to store misclassification rates
misclass_without_geo <- rep(NA, n_neighbors)
misclass_with_geo <- rep(NA, n_neighbors)

# Loop through different values of k (number of neighbors)
for (k in 1:n_neighbors) {
  
  # K-NN classification with only Height_max and SITE_ID
  knn_basic <- knn.cv(train = bird_data_scaled[, c("Height_max", "SITE_ID")], cl = bird_data$SPECIES, k = k)
  misclass_without_geo[k] <- mean(knn_basic != bird_data$SPECIES)
  
  # K-NN classification with Height_max, SITE_ID, Longitude, and Latitude
  knn_with_geo <- knn.cv(train = bird_data_scaled[, c("Height_max", "SITE_ID", "Longitude", "Latitude")], cl = bird_data$SPECIES, k = k)
  misclass_with_geo[k] <- mean(knn_with_geo != bird_data$SPECIES)
}

# Plot the misclassification rates for models with and without geographic coordinates
plot(1:n_neighbors, misclass_without_geo, type = "b", col = "blue", lwd = 2,
     xlab = "Number of Nearest Neighbors (k)", ylab = "Misclassification Rate",
     main = "Misclassification Rate by Number of Nearest Neighbors",
     ylim = range(c(misclass_without_geo, misclass_with_geo)))

# Add the points for the misclassification rate with geographic coordinates
points(1:n_neighbors, misclass_with_geo, type = "b", col = "red", lwd = 2)

# Add a legend to differentiate the two lines
legend("topright", legend = c("Without Geographic Coordinates", "With Geographic Coordinates"),
       col = c("blue", "red"), lwd = 2)




# Initialize vectors to store hit rates
hit_rate_without_geo <- rep(NA, n_neighbors)
hit_rate_with_geo <- rep(NA, n_neighbors)

# Loop through different values of k (number of neighbors)
for (k in 1:n_neighbors) {
  
  # K-NN classification with only Height_max and SITE_ID
  knn_basic <- knn.cv(train = bird_data_scaled[, c("Height_max", "SITE_ID")], cl = bird_data$SPECIES, k = k)
  hit_rate_without_geo[k] <- mean(knn_basic == bird_data$SPECIES)
  
  # K-NN classification with Height_max, SITE_ID, Longitude, and Latitude
  knn_with_geo <- knn.cv(train = bird_data_scaled[, c("Height_max", "SITE_ID", "Longitude", "Latitude")], cl = bird_data$SPECIES, k = k)
  hit_rate_with_geo[k] <- mean(knn_with_geo == bird_data$SPECIES)
}

# Plot the hit rates for models with and without geographic coordinates
plot(1:n_neighbors, hit_rate_without_geo, type = "b", col = "blue", lwd = 2,
     xlab = "Number of Nearest Neighbors (k)", ylab = "Hit Rate",
     main = "Hit Rate by Number of Nearest Neighbors",
     ylim = range(c(hit_rate_without_geo, hit_rate_with_geo)))
points(1:n_neighbors, hit_rate_with_geo, type = "b", col = "red", lwd = 2)
legend("bottomright", legend = c("Without Geographic Coordinates", "With Geographic Coordinates"),
       col = c("blue", "red"), lwd = 2)




# Minimum and maximum hit rate without geographic coordinates
min_hit_rate_without_geo <- min(hit_rate_without_geo)
max_hit_rate_without_geo <- max(hit_rate_without_geo)

# Minimum and maximum hit rate with geographic coordinates
min_hit_rate_with_geo <- min(hit_rate_with_geo)
max_hit_rate_with_geo <- max(hit_rate_with_geo)

# Minimum and maximum misclassification rate without geographic coordinates
min_misclassification_rate_without_geo <- 1 - max_hit_rate_without_geo
max_misclassification_rate_without_geo <- 1 - min_hit_rate_without_geo

# Minimum and maximum misclassification rate with geographic coordinates
min_misclassification_rate_with_geo <- 1 - max_hit_rate_with_geo
max_misclassification_rate_with_geo <- 1 - min_hit_rate_with_geo

# Overall (average) hit rate and misclassification rate without geographic coordinates
overall_hit_rate_without_geo <- mean(hit_rate_without_geo)
overall_misclassification_rate_without_geo <- 1 - overall_hit_rate_without_geo

# Overall (average) hit rate and misclassification rate with geographic coordinates
overall_hit_rate_with_geo <- mean(hit_rate_with_geo)
overall_misclassification_rate_with_geo <- 1 - overall_hit_rate_with_geo

# Print the results
cat("Without Geographic Coordinates:\n")
cat("Minimum hit rate:", min_hit_rate_without_geo, "\n")
cat("Maximum hit rate:", max_hit_rate_without_geo, "\n")
cat("Overall (average) hit rate:", overall_hit_rate_without_geo, "\n")
cat("Minimum misclassification rate:", min_misclassification_rate_without_geo, "\n")
cat("Maximum misclassification rate:", max_misclassification_rate_without_geo, "\n")
cat("Overall (average) misclassification rate:", overall_misclassification_rate_without_geo, "\n\n")

cat("With Geographic Coordinates:\n")
cat("Minimum hit rate:", min_hit_rate_with_geo, "\n")
cat("Maximum hit rate:", max_hit_rate_with_geo, "\n")
cat("Overall (average) hit rate:", overall_hit_rate_with_geo, "\n")
cat("Minimum misclassification rate:", min_misclassification_rate_with_geo, "\n")
cat("Maximum misclassification rate:", max_misclassification_rate_with_geo, "\n")
cat("Overall (average) misclassification rate:", overall_misclassification_rate_with_geo, "\n")


# Create a data frame to display the hit and misclassification rates with and without geographic coordinates
results_table <- data.frame(
  Metric = c(
    "Minimum Hit Rate", 
    "Maximum Hit Rate", 
    "Overall (Average) Hit Rate",
    "Minimum Misclassification Rate", 
    "Maximum Misclassification Rate", 
    "Overall (Average) Misclassification Rate"
  ),
  Without_Geographic_Coordinates = c(
    "0.0377 (3.77%)", 
    "0.0534 (5.34%)", 
    "0.0447 (4.47%)",
    "0.9466 (94.66%)", 
    "0.9623 (96.23%)", 
    "0.9553 (95.53%)"
  ),
  With_Geographic_Coordinates = c(
    "0.0389 (3.89%)", 
    "0.0540 (5.40%)", 
    "0.0455 (4.55%)",
    "0.9460 (94.60%)", 
    "0.9611 (96.11%)", 
    "0.9545 (95.45%)"
  )
)

# Display the table
print(results_table)







#-------------------------------------------------------------------------------------------------------------------------------------------------------------
#Question 3 with LDA , But not included in report, It's rought/ practice work/ Can Ignore this


#LDA model used
bird_data_clean_without_geographic <- subset(bird_data, 
                                             !is.na(SPECIES) & 
                                               !is.na(SITE_ID) &
                                               !is.na(Height_max) )
#Do the factorization because type of variables are vategorical
bird_data_clean_without_geographic$SPECIES <- as.factor(bird_data_clean_without_geographic$SPECIES)
bird_data_clean_without_geographic$SITE_ID <- as.factor(bird_data_clean_without_geographic$SITE_ID)

bird_data_clean_with_geographic <- subset(bird_data, 
                                          !is.na(SPECIES) & 
                                            !is.na(SITE_ID) &
                                            !is.na(Height_max) &
                                            !is.na(Longitude) &
                                            !is.na(Latitude))
#Do the factorization because type of variables are vategorical
bird_data_clean_with_geographic$SPECIES <- as.factor(bird_data_clean_with_geographic$SPECIES)
bird_data_clean_with_geographic$SITE_ID <- as.factor(bird_data_clean_with_geographic$SITE_ID)



bird.lda <- lda(SPECIES ~ Height_max + SITE_ID , data = bird_data_clean_without_geographic)

# Predict using the LDA model
lda_pred <- predict(bird.lda)
# Access the predicted classes
predictions <- lda_pred$class

length(predictions)
length(bird_data_clean_without_geographic$SPECIES)

# Produce the confusion matrix based on the LDA predictions
confusion_matrix <- table(predictions, bird_data_clean_without_geographic$SPECIES)
print(confusion_matrix)


# Calculate hit rate (accuracy)
hit_rate <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(hit_rate)
#Mis Classificartion Rate
misclassification_rate <- 1 - hit_rate
print(misclassification_rate)


#with Geographic Variables

bird.geographic.lda <- lda(SPECIES ~ Height_max + SITE_ID + Longitude + Latitude , data = bird_data_clean_with_geographic)

# Predict using the LDA model
lda_pred_geo <- predict(bird.geographic.lda)
# Access the predicted classes
predictions_geo <- lda_pred_geo$class

length(predictions_geo)
length(bird_data_clean_with_geographic$SPECIES)

# Produce the confusion matrix based on the LDA predictions
confusion_matrix_geo <- table(predictions_geo, bird_data_clean_with_geographic$SPECIES)
print(confusion_matrix_geo)

# Calculate hit rate (accuracy)
hit_rate_geo <- sum(diag(confusion_matrix_geo)) / sum(confusion_matrix_geo)
print(hit_rate_geo)
#Mis Classificartion Rate
misclassification_rate_geo <- 1 - hit_rate_geo
print(misclassification_rate_geo)




# Prepare data for plotting
results <- data.frame(
  Model = c("Without Geographic Variables", "With Geographic Variables"),
  Hit_Rate = c(hit_rate, hit_rate_geo),
  Misclassification_Rate = c(misclassification_rate, misclassification_rate_geo)
)

# Plot hit rate comparison
ggplot(results, aes(x = Model, y = Hit_Rate, fill = Model)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Hit Rate Comparison", y = "Hit Rate", x = "Model") +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  theme_minimal()

# Plot misclassification rate comparison
ggplot(results, aes(x = Model, y = Misclassification_Rate, fill = Model)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Misclassification Rate Comparison", y = "Misclassification Rate", x = "Model") +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  theme_minimal()



























