# ============================================
# NAIROBI REAL ESTATE - COMPLETE PREDICTIVE MODELING
# ============================================

# Load required libraries
library(tidyverse)
library(scales)
library(caret)
library(randomForest)
library(corrplot)


cat("========================================\n")
cat("NAIROBI REAL ESTATE - PREDICTIVE ANALYSIS\n")
cat("========================================\n\n")

# Prepare data (For Sale properties only)
properties_sale <- nairobi_data %>%
  filter(Status == "For Sale") %>%
  mutate(
    # Create numeric location scores based on average prices
    Location_Score = case_when(
      Location %in% c("Lavington", "Runda", "Karen") ~ 3,
      Location %in% c("Kileleshwa", "Kilimani") ~ 2,
      TRUE ~ 1
    )
  )

cat("Dataset prepared:", nrow(properties_sale), "properties\n\n")

# ============================================
# CORRELATION ANALYSIS
# ============================================

cat("========================================\n")
cat("FEATURE CORRELATION ANALYSIS\n")
cat("========================================\n\n")

# Select numeric variables for correlation
numeric_vars <- properties_sale %>%
  select(Price, Bedrooms, Bathrooms, Size_sqm, Location_Score) %>%
  na.omit()

# Calculate correlation matrix
correlation_matrix <- cor(numeric_vars, use = "complete.obs")

cat("CORRELATION WITH PRICE:\n")
cat("-----------------------\n")
price_correlations <- correlation_matrix[, "Price"] %>%
  sort(decreasing = TRUE)
print(round(price_correlations, 3))
cat("\n")

cat("Full Correlation Matrix:\n")
print(round(correlation_matrix, 3))
cat("\n")

# Create correlation plot
png("7_correlation_matrix.png", width = 800, height = 800, res = 120)
corrplot(correlation_matrix, 
         method = "color", 
         type = "upper",
         addCoef.col = "black", 
         number.cex = 0.8,
         tl.col = "black", 
         tl.srt = 45,
         title = "Feature Correlation with Property Price",
         mar = c(0, 0, 2, 0),
         col = colorRampPalette(c("blue", "white", "red"))(200))
dev.off()

cat("✓ Correlation matrix saved: 7_correlation_matrix.png\n\n")

# ============================================
# FEATURE IMPORTANCE ANALYSIS
# ============================================

cat("========================================\n")
cat("FEATURE IMPACT ON PRICE\n")
cat("========================================\n\n")

# 1. Location Impact
location_impact <- properties_sale %>%
  group_by(Location) %>%
  summarise(
    avg_price = mean(Price),
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_price)) %>%
  head(10)

cat("TOP 10 LOCATIONS BY AVERAGE PRICE:\n")
cat("-----------------------------------\n")
print(location_impact %>%
        mutate(avg_price = comma(avg_price)))
cat("\n")

# 2. Bedroom Impact
bedroom_stats <- properties_sale %>%
  group_by(Bedrooms) %>%
  summarise(
    avg_price = mean(Price),
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(Bedrooms)

cat("PRICE BY NUMBER OF BEDROOMS:\n")
cat("----------------------------\n")
print(bedroom_stats %>%
        mutate(avg_price = comma(avg_price)))

bedroom_model <- lm(Price ~ Bedrooms, data = properties_sale)
bedroom_coef <- coef(bedroom_model)[2]
cat("\nEach additional bedroom adds approximately: KES", 
    comma(round(bedroom_coef, 0)), "\n\n")

# 3. Size Impact
cat("PRICE BY PROPERTY SIZE:\n")
cat("-----------------------\n")
size_model <- lm(Price ~ Size_sqm, data = properties_sale)
size_coef <- coef(size_model)[2]
cat("Each additional square meter adds: KES", 
    comma(round(size_coef, 0)), "\n")
cat("Price per square meter (average): KES", 
    comma(round(mean(properties_sale$Price / properties_sale$Size_sqm, na.rm = TRUE), 0)), 
    "\n\n")

# ============================================
# PREPARE DATA FOR MODELING
# ============================================

model_data <- properties_sale %>%
  select(Price, Location, Type, Bedrooms, Bathrooms, Size_sqm) %>%
  na.omit()

# Split data: 80% training, 20% testing
set.seed(123)
train_index <- createDataPartition(model_data$Price, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

cat("========================================\n")
cat("DATA SPLIT\n")
cat("========================================\n")
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

cat("Model Coefficients (Top Features):\n")
coef_summary <- summary(linear_model)$coefficients
significant_coefs <- coef_summary[coef_summary[, 4] < 0.05, ]
print(head(significant_coefs[order(abs(significant_coefs[, 1]), decreasing = TRUE), ], 10))
cat("\n")

# Make predictions
linear_predictions <- predict(linear_model, newdata = test_data)

# Calculate performance metrics
linear_mae <- mean(abs(test_data$Price - linear_predictions))
linear_rmse <- sqrt(mean((test_data$Price - linear_predictions)^2))
linear_r2 <- cor(test_data$Price, linear_predictions)^2
linear_accuracy <- mean(abs(test_data$Price - linear_predictions) / test_data$Price < 0.1) * 100

cat("PERFORMANCE METRICS:\n")
cat("--------------------\n")
cat("Mean Absolute Error: KES", comma(round(linear_mae, 0)), "\n")
cat("Root Mean Squared Error: KES", comma(round(linear_rmse, 0)), "\n")
cat("R-squared:", round(linear_r2, 4), 
    "(explains", round(linear_r2 * 100, 1), "% of price variance)\n")
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

cat("✓ Training complete!\n\n")

# Feature importance
cat("FEATURE IMPORTANCE (% Increase in MSE if removed):\n")
cat("--------------------------------------------------\n")
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

cat("PERFORMANCE METRICS:\n")
cat("--------------------\n")
cat("Mean Absolute Error: KES", comma(round(rf_mae, 0)), "\n")
cat("Root Mean Squared Error: KES", comma(round(rf_rmse, 0)), "\n")
cat("R-squared:", round(rf_r2, 4), 
    "(explains", round(rf_r2 * 100, 1), "% of price variance)\n")
cat("Accuracy (within 10%):", round(rf_accuracy, 1), "%\n\n")

# ============================================
# MODEL COMPARISON
# ============================================

cat("========================================\n")
cat("MODEL COMPARISON\n")
cat("========================================\n\n")

comparison <- data.frame(
  Metric = c("MAE (KES)", "RMSE (KES)", "R-squared", "Accuracy (%)"),
  Linear_Regression = c(
    comma(round(linear_mae, 0)),
    comma(round(linear_rmse, 0)),
    round(linear_r2, 4),
    round(linear_accuracy, 1)
  ),
  Random_Forest = c(
    comma(round(rf_mae, 0)),
    comma(round(rf_rmse, 0)),
    round(rf_r2, 4),
    round(rf_accuracy, 1)
  )
)

print(comparison)
cat("\n")

# Determine best model
best_model <- ifelse(rf_r2 > linear_r2, "Random Forest", "Linear Regression")
improvement <- abs(rf_r2 - linear_r2) / min(rf_r2, linear_r2) * 100

cat("🏆 BEST MODEL:", best_model, "\n")
cat("   Performance improvement:", round(improvement, 1), "%\n\n")

# ============================================
# CREATE VISUALIZATIONS
# ============================================

cat("Generating visualizations...\n")

# Prediction comparison plot
pred_comparison <- data.frame(
  Actual = test_data$Price,
  Linear_Pred = linear_predictions,
  RF_Pred = rf_predictions
)

p1 <- ggplot(pred_comparison, aes(x = Actual / 1000000)) +
  geom_point(aes(y = Linear_Pred / 1000000, color = "Linear Regression"), 
             alpha = 0.6, size = 2.5) +
  geom_point(aes(y = RF_Pred / 1000000, color = "Random Forest"), 
             alpha = 0.6, size = 2.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
              color = "black", size = 1) +
  labs(
    title = "Predicted vs Actual Property Prices",
    subtitle = paste("Random Forest R² =", round(rf_r2, 3), 
                     "| Linear Regression R² =", round(linear_r2, 3)),
    x = "Actual Price (Millions KES)",
    y = "Predicted Price (Millions KES)",
    color = "Model"
  ) +
  scale_color_manual(values = c("Linear Regression" = "#FF6B6B", 
                                "Random Forest" = "#4ECDC4")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  )

ggsave("prediction_comparison.png", p1, width = 10, height = 8, dpi = 300)
cat("✓ Prediction comparison saved: prediction_comparison.png\n")

# Feature importance plot
p2 <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "#4ECDC4") +
  geom_text(aes(label = round(Importance, 1)), hjust = -0.2, size = 4) +
  coord_flip() +
  labs(
    title = "Random Forest Feature Importance",
    subtitle = "Higher values = more important for predicting price",
    x = "Feature",
    y = "% Increase in MSE"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

ggsave("feature_importance.png", p2, width = 10, height = 6, dpi = 300)
cat("✓ Feature importance saved: feature_importance.png\n\n")

# ============================================
# SAVE MODEL
# ============================================

if (best_model == "Random Forest") {
  saveRDS(rf_model, "nairobi_price_model.rds")
} else {
  saveRDS(linear_model, "nairobi_price_model.rds")
}

cat("========================================\n")
cat("✓ ANALYSIS COMPLETE!\n")
cat("========================================\n\n")

cat("Generated Files:\n")
cat("  📊 7_correlation_matrix.png - Feature correlations\n")
cat("  📈 prediction_comparison.png - Model predictions vs actual\n")
cat("  🎯 feature_importance.png - Most important features\n")
cat("  💾 nairobi_price_model.rds - Saved", best_model, "model\n\n")

cat("========================================\n")
cat("HOW TO USE THE SAVED MODEL\n")
cat("========================================\n\n")

cat("# Load the model\n")
cat("model <- readRDS('nairobi_price_model.rds')\n\n")

cat("# Predict price for a new property\n")
cat("new_property <- data.frame(\n")
cat("  Location = 'Westlands',\n")
cat("  Type = 'Apartment',\n")
cat("  Bedrooms = 3,\n")
cat("  Bathrooms = 2,\n")
cat("  Size_sqm = 120\n")
cat(")\n\n")

cat("predicted_price <- predict(model, newdata = new_property)\n")
cat("cat('Predicted Price: KES', format(round(predicted_price), big.mark=','))\n")