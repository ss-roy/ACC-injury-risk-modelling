# -------------------------- Script Information --------------------------
# Purpose:
# This script builds a baseline Random Forest model (Model 1) to predict the 
# likelihood of gym-related injuries. Class imbalance is noted (with fewer 
# instances of "Y" or injury claims), but the model is still built to serve as 
# a baseline for understanding the factors that contribute to gym injury risks

# Key Steps:
#    - Selects relevant features for modeling, including demographic information 
#      (`age_at_extraction_date`, `ethnicity_last_claim`, etc.), gym injury counts, 
#      and total injuries. Rationale provided below.

#    - Converts the target variable (`y`) into a factor to ensure compatibility with 
#      classification models.
#    - Drops rows with missing values to ensure model input quality.

#    - Trains a Random Forest classifier (`rf_model`) with 100 decision trees 
#      to classify individuals as either likely ("Y") or unlikely ("N") to have a 
#      gym-related injury.

#    - Predicts the probability of gym injuries (`predicted_probability`) for each 
#      individual in the dataset, specifically focusing on the likelihood of the "Y" class.
#    - Appends the predicted probabilities to the original dataset.

#    - Prepares a sample output table for selected individuals, including their 
#      demographic details, predicted probabilities, and likelihood percentages.

# Note:
#   Model 1 serves as a baseline, but the severe class imbalance (few "Y" cases) 
#   limits its ability to predict rare outcomes effectively. This underscores the need 
#   for data balancing techniques or alternative approaches to improve performance 
#   on the minority class.

# Feature selection rationale:

# age_at_extraction_date: Age may influence the likelihood of injury due to differences in physical fitness, recovery time, and risk tolerance across age groups.
# ethnicity_last_claim: Ethnic background may be a proxy for underlying socioeconomic or cultural factors that influence gym usage patterns or injury risks.
# location_tla_last_claim: Geographic location might correlate with access to gyms, quality of gym facilities, or local physical activity trends.
# work_last_claim: Occupational activity level (e.g., sedentary vs. physically demanding jobs) could influence injury risk when engaging in gym activities.
# Areaunit_score: A socioeconomic index can reflect access to healthcare, lifestyle factors, and gym use patterns, which impact injury likelihood.
# num_gym_all: A count of previous gym-related injuries serves as a direct predictor of future injuries based on patterns of risky behavior or gym habits.
# num_wgt_all: Weightlifting injuries are relevant as they directly reflect gym-specific risks.
# total_injuries: The overall injury count indicates an individual's propensity for injuries, providing predictive power for future risks.\


# ------------------------------------------------------------------------

# Model 1: Baseline Random Forest ---
# # Select relevant features

model_data <- data %>%
  select(age_at_extraction_date, ethnicity_last_claim, location_tla_last_claim, work_last_claim,
         Areaunit_score, num_gym_all, num_wgt_all, total_injuries, y) %>%
  mutate(y = as.factor(y)) %>%  # Ensure target is a factor
  drop_na()  # Drop rows with missing values

# Train Random Forest
set.seed(123)
rf_model <- randomForest(y ~ ., data = model_data, ntree = 100, importance = TRUE)

# Evaluate Model 1
pred_prob <- predict(rf_model, data, type = "prob")[, 2]  # Predicted probabilities for "Y"

# Add predicted probabilities to the data
# Output Results for a Sample ---
# Generate output for an individual sample
data$predicted_probability <- pred_prob
