# ============================================
# NAIROBI REAL ESTATE - PREDICTIVE MODELING
# ============================================

# Load required libraries
library(tidyverse)
library(scales)
library(caret)
library(randomForest)


cat("========================================\n")
cat("PREDICTIVE MODELING - NAIROBI PROPERTIES\n")
cat("========================================\n\n")

# Prepare data (For Sale properties only)
properties_sale <- nairobi_data %>%
  filter(Status == "For Sale") %>%
  select(Price, Location, Type, Bedrooms, Bathrooms, Size_sqm) %>%
  na.omit()

cat("Dataset prepared:", nrow(properties_sale), "properties\n\n")

# ============================================
# SPLIT DATA: 80% Training, 20% Testing
# ============================================

set.seed(123)
train_index <- createDataPartition(properties_sale$Price, p = 0.8, list = FALSE)
train_data <- properties_sale[train_index, ]
test_data <- properties_sale[-train_index, ]

cat("Training set:", nrow(train_data), "properties\n")
cat("Testing set:", nrow(test_data), "properties\n\n")

# ============================================
# MODEL 1: LINEAR REGRESSION
# ============================================

cat("========================================\n")
cat("MODEL 1: LINEAR REGRESSION\n")
cat("========================================\n\n")

linear_model <- lm(Price ~ Location + Type + Bedrooms + Bathrooms + Size_sqm, 
                   data = train_data)

cat("Model Summary:\n")
print(summary(linear_model))

# Make predictions
linear_predictions <- predict(linear_model, newdata = test_data)

# Calculate performance metrics
linear_mae <- mean(abs(test_data$Price - linear_predictions))
linear_rmse <- sqrt(mean((test_data$Price - linear_predictions)^2))
linear_r2 <- cor(test_data$Price, linear_predictions)^2
linear_accuracy <- mean(abs(test_data$Price - linear_predictions) / test_data$Price < 0.1) * 100

cat("\n========================================\n")
cat("LINEAR REGRESSION PERFORMANCE:\n")
cat("========================================\n")
cat("Mean Absolute Error: KES", comma(round(linear_mae, 0)), "\n")
cat("Root Mean Squared Error: KES", comma(round(linear_rmse, 0)), "\n")
cat("R-squared:", round(linear_r2, 4), "\n")
cat("Accuracy (within 10%):", round(linear_accuracy, 1), "%\n\n")

# ============================================
# MODEL 2: RANDOM FOREST
# ============================================

cat("========================================\n")
cat("MODEL 2: RANDOM FOREST\n")
cat("========================================\n\n")

cat("Training Random Forest with 100 trees...\n")
rf_model <- randomForest(Price ~ Location + Type + Bedrooms + Bathrooms + Size_sqm,
                         data = train_data,
                         ntree = 100,
                         importance = TRUE)

cat("Training complete!\n\n")

# Feature importance
cat("FEATURE IMPORTANCE:\n")
cat("-------------------\n")
importance_scores <- importance(rf_model)
importance_df <- data.frame(
  Feature = rownames(importance_scores),
  Importance = importance_scores[, 1]
) %>%
  arrange(desc(Importance))

print(importance_df)
cat("\n")

# Make predictions
rf_predictions <- predict(rf_model, newdata = test_data)

# Calculate performance metrics
rf_mae <- mean(abs(test_data$Price - rf_predictions))
rf_rmse <- sqrt(mean((test_data$Price - rf_predictions)^2))
rf_r2 <- cor(test_data$Price, rf_predictions)^2
rf_accuracy <- mean(abs(test_data$Price - rf_predictions) / test_data$Price < 0.1) * 100

cat("========================================\n")
cat("RANDOM FOREST PERFORMANCE:\n")
cat("========================================\n")
cat("Mean Absolute Error: KES", comma(round(rf_mae, 0)), "\n")
cat("Root Mean Squared Error: KES", comma(round(rf_rmse, 0)), "\n")
cat("R-squared:", round(rf_r2, 4), "\n")
cat("Accuracy (within 10%):", round(rf_accuracy, 1), "%\n\n")

# ============================================
# MODEL COMPARISON
# ============================================

cat("========================================\n")
cat("MODEL COMPARISON\n")
cat("========================================\n\n")

comparison <- data.frame(
  Model = c("Linear Regression", "Random Forest"),
  MAE = c(comma(round(linear_mae, 0)), comma(round(rf_mae, 0))),
  RMSE = c(comma(round(linear_rmse, 0)), comma(round(rf_rmse, 0))),
  R_squared = c(round(linear_r2, 4), round(rf_r2, 4)),
  Accuracy = c(paste0(round(linear_accuracy, 1), "%"), 
               paste0(round(rf_accuracy, 1), "%"))
)

print(comparison)

# Determine best model
best_model <- ifelse(rf_r2 > linear_r2, "Random Forest", "Linear Regression")
cat("\nBest performing model:", best_model, "\n\n")

# ============================================
# CREATE VISUALIZATION
# ============================================

pred_comparison <- data.frame(
  Actual = test_data$Price,
  Linear_Pred = linear_predictions,
  RF_Pred = rf_predictions
)

p <- ggplot(pred_comparison, aes(x = Actual / 1000000)) +
  geom_point(aes(y = Linear_Pred / 1000000, color = "Linear Regression"), 
             alpha = 0.6, size = 2) +
  geom_point(aes(y = RF_Pred / 1000000, color = "Random Forest"), 
             alpha = 0.6, size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
              color = "black", size = 1) +
  labs(
    title = "Predicted vs Actual Property Prices",
    subtitle = "Comparison of Linear Regression and Random Forest Models",
    x = "Actual Price (Millions KES)",
    y = "Predicted Price (Millions KES)",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  )

ggsave("prediction_comparison.png", p, width = 10, height = 8, dpi = 300)

cat("========================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("========================================\n\n")

cat("Generated files:\n")
cat("  - prediction_comparison.png\n")
cat("  - nairobi_price_model.rds (saved model)\n\n")

# Save the best performing model
if (best_model == "Random Forest") {
  saveRDS(rf_model, "nairobi_price_model.rds")
} else {
  saveRDS(linear_model, "nairobi_price_model.rds")
}

cat("========================================\n")
cat("HOW TO USE THE SAVED MODEL:\n")
cat("========================================\n\n")

cat("# Load the model\n")
cat("model <- readRDS('nairobi_price_model.rds')\n\n")

cat("# Make a prediction for a new property\n")
cat("new_property <- data.frame(\n")
cat("  Location = 'Westlands',\n")
cat("  Type = 'Apartment',\n")
cat("  Bedrooms = 3,\n")
cat("  Bathrooms = 2,\n")
cat("  Size_sqm = 120\n")
cat(")\n\n")

cat("predicted_price <- predict(model, newdata = new_property)\n")
cat("cat('Predicted Price: KES', format(predicted_price, big.mark=','), '\\n')\n")