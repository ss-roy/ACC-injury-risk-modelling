# -------------------------- Script Information --------------------------
# Purpose:
# This script performs data cleaning and preprocessing on a dataset to prepare it for 
# exploratory data analysis (EDA) and predictive modeling. It handles outliers, missing 
# values, and feature engineering, ensuring data consistency for modeling tasks.

# Key Steps:
# 1. Load Necessary Libraries:
# 2. Load Dataset
# 3. Data Cleaning:
#    a. Removing Outliers:
#       - Filters out rows with unrealistic ages (negative values or ages > 116).
#    b. Handling Missing Values:
#       - Replaces missing `Areaunit_score` with 99 (default placeholder).
#       - Fills `NA` values in numeric injury-related columns with 0.
#    c. Standardizing Date Information:
#       - Extracts the year ending in June (e.g., "2013") from `acci_year` and stores 
#         it as `YEJune_last_claim`.
#       - Computes a future claim date (`next_claim`) by adding 12 months to 
#         `YEJune_last_claim`. - Not used anywhere
#    d. Feature Engineering:
#       - Creates a new column `total_injuries` by summing all numeric columns related 
#         to injury counts (columns starting with "num_" or ending with "_all").
#    e. Handling Categorical Variables:
#       - For the `work_last_claim` column:
#         - Replaces missing values with "Not defined".
#         - Converts the column back to a factor.
#   4. Check for mulitcollinearity and features that could potentially go into the model

# Output:
# - A cleaned and preprocessed dataset 
# ------------------------------------------------------------------------

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(randomForest)
library(caret)
library(zoo)
library(pROC)
library(smotefamily)
library(fastDummies)
library(corrplot)
library(gridExtra)
library(plotly)
library(xgboost)



# Data Cleaning and Preprocessing ---
# Load the dataset
data <- read.csv("data/DataForExercise.csv")

data <- data %>%
  # Remove outliers in age (negative values or age > 116)
  filter(age_at_extraction_date >= 15 & age_at_extraction_date <= 80) %>%
  # Handle missing values
  mutate(
    Areaunit_score = ifelse(is.na(Areaunit_score), 99, Areaunit_score),
    across(where(is.numeric), ~ replace_na(., 0)),  # Replace NA in numeric injury columns with 0
    YEJune_last_claim = sub(".*?/(.*)", "\\1", acci_year),  # Extract year ending in June
    next_claim = as.Date(as.yearmon(YEJune_last_claim)) + months(12),  # Add 12 months
    total_injuries = rowSums(select(., starts_with("num_") | ends_with("_all")), na.rm = TRUE)  # Total injuries
  )

# Replace missing values in work_last_claim
data$work_last_claim <- as.character(data$work_last_claim)
data$work_last_claim[is.na(data$work_last_claim)] <- "Not defined"
data$work_last_claim <- factor(data$work_last_claim)

# Features that could be used
cor_matrix <- cor(data %>% select_if(is.numeric))
high_corr <- findCorrelation(cor_matrix, cutoff = 0.90)
selected_features <- colnames(cor_matrix)[-high_corr]

# Convert non-numeric features to factors
non_numeric_cols <- sapply(data, function(x) !is.numeric(x))
data[, non_numeric_cols] <- lapply(data[, non_numeric_cols], as.factor)

# Check for missing data and select features with < 20% missing values
missing_data <- colSums(is.na(data)) / nrow(data) * 100
usable_features <- names(missing_data[missing_data < 20])

# Potential features
features <- intersect(selected_features, usable_features)

