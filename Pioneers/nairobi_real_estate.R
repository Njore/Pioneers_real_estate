# ============================================
# COMPREHENSIVE NAIROBI REAL ESTATE ANALYSIS
# Complete Analysis to Fill All Report Placeholders
# ============================================

# Load required libraries
library(tidyverse)
library(scales)
library(lubridate)

# Read the dataset
nairobi_data <- read_csv("nairobi_properties.csv")

cat("========================================\n")
cat("NAIROBI REAL ESTATE COMPREHENSIVE ANALYSIS\n")
cat("========================================\n\n")

# ============================================
# SECTION 1: BASIC PROPERTY ANALYSIS
# ============================================

cat("=== SECTION 1: BASIC PROPERTY ANALYSIS ===\n\n")

# Separate properties into For Sale and For Rent
properties_for_sale <- nairobi_data %>%
  filter(Status == "For Sale")

properties_for_rent <- nairobi_data %>%
  filter(Status == "For Rent")

cat("Total Properties:", nrow(nairobi_data), "\n")
cat("Properties For Sale:", nrow(properties_for_sale), "\n")
cat("Properties For Rent:", nrow(properties_for_rent), "\n\n")

# Calculate descriptive statistics for sale prices
sale_stats <- properties_for_sale %>%
  summarise(
    mean_price = mean(Price, na.rm = TRUE),
    median_price = median(Price, na.rm = TRUE),
    min_price = min(Price, na.rm = TRUE),
    max_price = max(Price, na.rm = TRUE),
    sd_price = sd(Price, na.rm = TRUE),
    count = n()
  )

cat("PROPERTY PRICE STATISTICS (KES) - FOR SALE:\n")
cat("--------------------------------------------\n")
cat("Average property price: KES", comma(sale_stats$mean_price), "\n")
cat("Median property price: KES", comma(sale_stats$median_price), "\n")
cat("Minimum property price: KES", comma(sale_stats$min_price), "\n")
cat("Maximum property price: KES", comma(sale_stats$max_price), "\n")
cat("Standard deviation: KES", comma(sale_stats$sd_price), "\n")
cat("Total properties (For Sale):", sale_stats$count, "\n\n")

# Calculate skewness
price_skew <- (sale_stats$mean_price - sale_stats$median_price) / sale_stats$sd_price
price_cv <- sale_stats$sd_price / sale_stats$mean_price

cat("Distribution Skewness:", round(price_skew, 3), "\n")
cat("Coefficient of Variation:", round(price_cv, 3), "\n\n")

# Rental statistics
if (nrow(properties_for_rent) > 0) {
  rental_stats <- properties_for_rent %>%
    summarise(
      mean_rental = mean(Price, na.rm = TRUE),
      median_rental = median(Price, na.rm = TRUE),
      min_rental = min(Price, na.rm = TRUE),
      max_rental = max(Price, na.rm = TRUE),
      sd_rental = sd(Price, na.rm = TRUE),
      count = n()
    )
  
  cat("RENTAL RATE STATISTICS (Monthly KES):\n")
  cat("--------------------------------------\n")
  cat("Average monthly rental rate: KES", comma(rental_stats$mean_rental), "\n")
  cat("Median monthly rental rate: KES", comma(rental_stats$median_rental), "\n")
  cat("Minimum rental rate: KES", comma(rental_stats$min_rental), "\n")
  cat("Maximum rental rate: KES", comma(rental_stats$max_rental), "\n")
  cat("Total rental properties:", rental_stats$count, "\n\n")
}

# ============================================
# SECTION 2: ANALYSIS BY PROPERTY TYPE
# ============================================

cat("\n=== SECTION 2: ANALYSIS BY PROPERTY TYPE ===\n\n")

# Property type analysis for sale properties
type_analysis <- properties_for_sale %>%
  group_by(Type) %>%
  summarise(
    avg_price = mean(Price),
    median_price = median(Price),
    total_value = sum(Price),
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    pct_of_market = (count / sum(count)) * 100,
    pct_of_value = (total_value / sum(total_value)) * 100
  ) %>%
  arrange(desc(total_value))

cat("PROPERTY TYPE BREAKDOWN:\n")
cat("------------------------\n")
print(type_analysis %>%
        mutate(
          avg_price = comma(avg_price),
          median_price = comma(median_price),
          total_value = comma(total_value),
          pct_of_market = paste0(round(pct_of_market, 1), "%"),
          pct_of_value = paste0(round(pct_of_value, 1), "%")
        ))

cat("\n")

# ============================================
# SECTION 3: ANALYSIS BY LOCATION
# ============================================

cat("\n=== SECTION 3: ANALYSIS BY LOCATION ===\n\n")

# Overall location counts
location_counts <- nairobi_data %>%
  count(Location, sort = TRUE)

cat("TOP 5 LOCATIONS BY NUMBER OF PROPERTIES:\n")
cat("-----------------------------------------\n")
print(head(location_counts, 5))
cat("\n")

# Location analysis for sale properties
location_analysis <- properties_for_sale %>%
  group_by(Location) %>%
  summarise(
    avg_price = mean(Price),
    median_price = median(Price),
    min_price = min(Price),
    max_price = max(Price),
    total_value = sum(Price),
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    pct_of_market = (count / sum(count)) * 100,
    price_range = max_price - min_price
  ) %>%
  arrange(desc(avg_price))

cat("TOP 5 MOST EXPENSIVE LOCATIONS (Average Sale Price):\n")
cat("-----------------------------------------------------\n")
print(location_analysis %>%
        head(5) %>%
        mutate(
          avg_price = comma(avg_price),
          median_price = comma(median_price),
          total_value = comma(total_value)
        ) %>%
        select(Location, avg_price, median_price, count, total_value))

cat("\n")

# Full location comparison table
cat("COMPLETE LOCATION COMPARISON TABLE:\n")
cat("------------------------------------\n")
location_table <- location_analysis %>%
  mutate(
    avg_price_fmt = comma(avg_price),
    total_value_fmt = comma(total_value),
    pct_market_fmt = paste0(round(pct_of_market, 1), "%"),
    min_price_fmt = comma(min_price),
    max_price_fmt = comma(max_price)
  ) %>%
  select(Location, avg_price_fmt, total_value_fmt, count, 
         pct_market_fmt, min_price_fmt, max_price_fmt)

print(location_table)
cat("\n")

# ============================================
# SECTION 4: PROPERTY FEATURES ANALYSIS
# ============================================

cat("\n=== SECTION 4: PROPERTY FEATURES ANALYSIS ===\n\n")

# Size analysis
cat("SIZE ANALYSIS:\n")
cat("--------------\n")
size_stats <- properties_for_sale %>%
  summarise(
    min_size = min(Size_sqm),
    max_size = max(Size_sqm),
    avg_size = mean(Size_sqm),
    median_size = median(Size_sqm)
  )

cat("Minimum size:", size_stats$min_size, "sq m\n")
cat("Maximum size:", size_stats$max_size, "sq m\n")
cat("Average size:", round(size_stats$avg_size, 1), "sq m\n")
cat("Median size:", size_stats$median_size, "sq m\n\n")

# Correlation between size and price
size_price_corr <- cor(properties_for_sale$Size_sqm, properties_for_sale$Price)
cat("Size-Price Correlation:", round(size_price_corr, 3), "\n\n")

# Size categories
properties_for_sale <- properties_for_sale %>%
  mutate(
    size_category = case_when(
      Size_sqm < 100 ~ "Small (<100 sqm)",
      Size_sqm >= 100 & Size_sqm < 150 ~ "Medium (100-150 sqm)",
      Size_sqm >= 150 ~ "Large (>150 sqm)"
    )
  )

size_category_analysis <- properties_for_sale %>%
  group_by(size_category) %>%
  summarise(
    avg_price = mean(Price),
    count = n(),
    price_per_sqm = mean(Price / Size_sqm),
    .groups = "drop"
  )

cat("PRICE BY SIZE CATEGORY:\n")
print(size_category_analysis %>%
        mutate(
          avg_price = comma(avg_price),
          price_per_sqm = comma(round(price_per_sqm, 0))
        ))
cat("\n")

# Bedroom analysis
cat("BEDROOM ANALYSIS:\n")
cat("-----------------\n")
bedroom_analysis <- properties_for_sale %>%
  group_by(Bedrooms) %>%
  summarise(
    avg_price = mean(Price),
    median_price = median(Price),
    count = n(),
    pct_of_market = (n() / nrow(properties_for_sale)) * 100,
    .groups = "drop"
  ) %>%
  arrange(Bedrooms)

print(bedroom_analysis %>%
        mutate(
          avg_price = comma(avg_price),
          median_price = comma(median_price),
          pct_of_market = paste0(round(pct_of_market, 1), "%")
        ))
cat("\n")

# Bathroom analysis
cat("BATHROOM ANALYSIS:\n")
cat("------------------\n")
bathroom_analysis <- properties_for_sale %>%
  group_by(Bathrooms) %>%
  summarise(
    avg_price = mean(Price),
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(Bathrooms)

print(bathroom_analysis %>%
        mutate(avg_price = comma(avg_price)))
cat("\n")

# ============================================
# SECTION 5: TIME-BASED ANALYSIS
# ============================================

cat("\n=== SECTION 5: TIME-BASED ANALYSIS ===\n\n")

# Convert DateListed to date format
nairobi_data <- nairobi_data %>%
  mutate(DateListed = as.Date(DateListed))

properties_for_sale <- properties_for_sale %>%
  mutate(DateListed = as.Date(DateListed))

# Get first and last months
date_range <- properties_for_sale %>%
  summarise(
    first_date = min(DateListed),
    last_date = max(DateListed)
  )

cat("Analysis Period:", format(date_range$first_date, "%B %Y"), 
    "to", format(date_range$last_date, "%B %Y"), "\n\n")

# First month analysis
first_month <- floor_date(date_range$first_date, "month")
last_month <- floor_date(date_range$last_date, "month")

first_month_data <- properties_for_sale %>%
  filter(floor_date(DateListed, "month") == first_month) %>%
  summarise(
    total_value = sum(Price),
    count = n(),
    avg_price = mean(Price)
  )

last_month_data <- properties_for_sale %>%
  filter(floor_date(DateListed, "month") == last_month) %>%
  summarise(
    total_value = sum(Price),
    count = n(),
    avg_price = mean(Price)
  )

cat("OVERALL MARKET TRENDS:\n")
cat("----------------------\n")
cat("First Month (", format(first_month, "%B %Y"), "):\n", sep = "")
cat("  Total Value: KES", comma(first_month_data$total_value), "\n")
cat("  Transactions:", first_month_data$count, "\n")
cat("  Average Price: KES", comma(first_month_data$avg_price), "\n\n")

cat("Last Month (", format(last_month, "%B %Y"), "):\n", sep = "")
cat("  Total Value: KES", comma(last_month_data$total_value), "\n")
cat("  Transactions:", last_month_data$count, "\n")
cat("  Average Price: KES", comma(last_month_data$avg_price), "\n\n")

# Calculate changes
value_change <- ((last_month_data$total_value - first_month_data$total_value) / 
                   first_month_data$total_value) * 100
trans_change <- ((last_month_data$count - first_month_data$count) / 
                   first_month_data$count) * 100
price_change <- ((last_month_data$avg_price - first_month_data$avg_price) / 
                   first_month_data$avg_price) * 100

cat("CHANGES FROM FIRST TO LAST MONTH:\n")
cat("Value Change:", sprintf("%.1f%%", value_change), "\n")
cat("Transaction Change:", sprintf("%.1f%%", trans_change), "\n")
cat("Average Price Change:", sprintf("%.1f%%", price_change), "\n\n")

# Location-specific time analysis
cat("LOCATION-SPECIFIC TIME TRENDS:\n")
cat("------------------------------\n")

time_by_location <- properties_for_sale %>%
  mutate(month = floor_date(DateListed, "month")) %>%
  filter(month %in% c(first_month, last_month)) %>%
  group_by(Location, month) %>%
  summarise(
    total_value = sum(Price),
    count = n(),
    avg_price = mean(Price),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = month,
    values_from = c(total_value, count, avg_price),
    names_sep = "_"
  )

# Calculate changes for each location
location_time <- time_by_location %>%
  mutate(
    value_change_pct = ((get(paste0("total_value_", last_month)) - 
                           get(paste0("total_value_", first_month))) / 
                          get(paste0("total_value_", first_month))) * 100,
    trans_change_pct = ((get(paste0("count_", last_month)) - 
                           get(paste0("count_", first_month))) / 
                          get(paste0("count_", first_month))) * 100
  ) %>%
  arrange(desc(value_change_pct))

print(location_time %>%
        head(10) %>%
        select(Location, contains("total_value"), contains("count"), 
               value_change_pct, trans_change_pct) %>%
        mutate(across(contains("total_value"), comma),
               value_change_pct = paste0(round(value_change_pct, 1), "%"),
               trans_change_pct = paste0(round(trans_change_pct, 1), "%")))

cat("\n")

# ============================================
# SECTION 6: PRICE RANGE SEGMENTATION
# ============================================

cat("\n=== SECTION 6: PRICE RANGE SEGMENTATION ===\n\n")

# Define price brackets based on data distribution
price_brackets <- quantile(properties_for_sale$Price, probs = c(0, 0.33, 0.67, 1))

properties_for_sale <- properties_for_sale %>%
  mutate(
    price_segment = case_when(
      Price < price_brackets[2] ~ "Budget",
      Price >= price_brackets[2] & Price < price_brackets[3] ~ "Mid-Range",
      Price >= price_brackets[3] ~ "Luxury"
    )
  )

price_segmentation <- properties_for_sale %>%
  group_by(price_segment) %>%
  summarise(
    count = n(),
    total_value = sum(Price),
    avg_price = mean(Price),
    min_price = min(Price),
    max_price = max(Price),
    .groups = "drop"
  ) %>%
  mutate(
    pct_of_market = (count / sum(count)) * 100,
    pct_of_value = (total_value / sum(total_value)) * 100
  )

cat("PRICE RANGE CATEGORIES:\n")
cat("-----------------------\n")
print(price_segmentation %>%
        mutate(
          count = count,
          pct_of_market = paste0(round(pct_of_market, 1), "%"),
          total_value = comma(total_value),
          avg_price = comma(avg_price),
          min_price = comma(min_price),
          max_price = comma(max_price),
          pct_of_value = paste0(round(pct_of_value, 1), "%")
        ))

cat("\n")
cat("Price Bracket Thresholds:\n")
cat("Budget: < KES", comma(price_brackets[2]), "\n")
cat("Mid-Range: KES", comma(price_brackets[2]), "- KES", comma(price_brackets[3]), "\n")
cat("Luxury: > KES", comma(price_brackets[3]), "\n\n")

# ============================================
# SECTION 7: INVESTMENT INDICATORS
# ============================================

cat("\n=== SECTION 7: INVESTMENT INDICATORS ===\n\n")
cat("NOTE: Full ROI and rental yield calculations require additional data\n")
cat("      (purchase costs, operating expenses, market appreciation rates).\n")
cat("      Basic indicators provided based on available data.\n\n")

# Estimate rental yield for locations with both sale and rent data
investment_by_location <- properties_for_sale %>%
  group_by(Location) %>%
  summarise(
    avg_sale_price = mean(Price),
    sale_count = n(),
    .groups = "drop"
  )

rental_by_location <- properties_for_rent %>%
  group_by(Location) %>%
  summarise(
    avg_rental = mean(Price),
    rental_count = n(),
    .groups = "drop"
  )

investment_indicators <- investment_by_location %>%
  left_join(rental_by_location, by = "Location") %>%
  filter(!is.na(avg_rental)) %>%
  mutate(
    estimated_annual_yield = (avg_rental * 12 / avg_sale_price) * 100
  ) %>%
  arrange(desc(estimated_annual_yield))

cat("ESTIMATED RENTAL YIELD BY LOCATION:\n")
cat("(Annual rental income / property value)\n")
cat("------------------------------------\n")
print(investment_indicators %>%
        mutate(
          avg_sale_price = comma(avg_sale_price),
          avg_rental = comma(avg_rental),
          estimated_annual_yield = paste0(round(estimated_annual_yield, 2), "%")
        ) %>%
        select(Location, avg_sale_price, avg_rental, estimated_annual_yield))

cat("\n")

# ============================================
# CREATE VISUALIZATIONS
# ============================================

cat("\n=== CREATING VISUALIZATIONS ===\n\n")

# 1. Price distribution
p1 <- ggplot(properties_for_sale, aes(x = Price / 1000000)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
  labs(
    title = "Distribution of Property Prices in Nairobi (For Sale)",
    subtitle = paste0("n = ", nrow(properties_for_sale), " properties"),
    x = "Price (Millions KES)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

ggsave("1_price_distribution.png", p1, width = 10, height = 6, dpi = 300)

# 2. Price by location
p2 <- ggplot(properties_for_sale, aes(x = reorder(Location, Price), y = Price / 1000000)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Property Prices by Location (For Sale)",
    x = "Location",
    y = "Price (Millions KES)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

ggsave("2_price_by_location.png", p2, width = 10, height = 8, dpi = 300)

# 3. Price by property type
p3 <- ggplot(properties_for_sale, aes(x = reorder(Type, Price), y = Price / 1000000)) +
  geom_boxplot(fill = "coral", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Property Prices by Type (For Sale)",
    x = "Property Type",
    y = "Price (Millions KES)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

ggsave("3_price_by_type.png", p3, width = 10, height = 6, dpi = 300)

# 4. Bedroom analysis
p4 <- ggplot(bedroom_analysis, aes(x = factor(Bedrooms), y = avg_price / 1000000)) +
  geom_col(fill = "darkgreen", alpha = 0.7) +
  geom_text(aes(label = paste0("n=", count)), vjust = -0.5, size = 3) +
  labs(
    title = "Average Price by Number of Bedrooms",
    x = "Number of Bedrooms",
    y = "Average Price (Millions KES)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

ggsave("4_price_by_bedrooms.png", p4, width = 10, height = 6, dpi = 300)

# 5. Price segmentation
p5 <- ggplot(price_segmentation, aes(x = "", y = count, fill = price_segment)) +
  geom_col(width = 1) +
  coord_polar("y") +
  labs(title = "Market Segmentation by Price Range",
       fill = "Segment") +
  theme_void() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

ggsave("5_price_segmentation.png", p5, width = 8, height = 8, dpi = 300)

# 6. Rental distribution
if (nrow(properties_for_rent) > 0) {
  p6 <- ggplot(properties_for_rent, aes(x = Price / 1000)) +
    geom_histogram(bins = 20, fill = "purple", color = "white", alpha = 0.8) +
    labs(
      title = "Distribution of Rental Rates in Nairobi",
      subtitle = paste0("n = ", nrow(properties_for_rent), " properties"),
      x = "Monthly Rent (Thousands KES)",
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  ggsave("6_rental_distribution.png", p6, width = 10, height = 6, dpi = 300)
}

cat("Visualizations saved successfully!\n")
cat("Files created:\n")
cat("  - 1_price_distribution.png\n")
cat("  - 2_price_by_location.png\n")
cat("  - 3_price_by_type.png\n")
cat("  - 4_price_by_bedrooms.png\n")
cat("  - 5_price_segmentation.png\n")
if (nrow(properties_for_rent) > 0) {
  cat("  - 6_rental_distribution.png\n")
}

cat("\n========================================\n")
cat("COMPREHENSIVE ANALYSIS COMPLETE!\n")
cat("========================================\n")
