# -------------------------- Script Information --------------------------
# Purpose:
# This script builds and evaluates an enhanced Random Forest model (Model 2) to address the 
# challenges of class imbalance in predicting gym-related injuries. It incorporates the 
# Synthetic Minority Oversampling Technique (SMOTE) to create a balanced dataset and improve 
# the model's ability to identify rare outcomes. While the class imbalance issue persists 
# even after SMOTE, this approach could be further refined by tuning parameters to potentially 
# create an even more balanced dataset. Early results suggest improved predictive power, 
# demonstrating a promising direction for more accurate and fair predictions of gym injury risks.

# Key Steps:
#    - Separates the features (X) and target variable (`y`) for modeling.
#    - Ensures all features are numeric, applying one-hot encoding to categorical variables 
#      where necessary, as SMOTE requires numeric data.

#    - Balances the dataset by generating synthetic examples for the minority class (`Yes` in `y`) 
#      using SMOTE.
#    - Adds the balanced target variable (`y`) back into the dataset for modeling.

#    - Trains a Random Forest model (`rf_model_balanced`) using the SMOTE-balanced dataset.
#    - Excludes non-predictive columns such as `PersonACCId`, `class`, or `index_id` to focus on 
#      relevant features.
#    - Predicts probabilities and class labels for the SMOTE-balanced dataset.
#    - Combines predictions with the original dataset for review and comparison.

# Highlights:
# - The use of SMOTE improves the model's performance on the minority class by reducing the 
#   imbalance between "Yes" and "No" cases.
# - This allows for more accurate identification of individuals at high risk for gym injuries, 
#   as evidenced by improved recall and F1-Score in model diagnostics.


# ------------------------------------------------------------------------


# Step 1: Prepare Data for SMOTE
# Extract features (X) and the target variable (y) from the cleaned dataset
if ("original_row" %in% colnames(data)) {
  X <- data %>%
    select(-y, -original_row)  # Exclude 'y' and 'original_row' if present
} else {
  X <- data %>%
    select(-y)  # Exclude only 'y' if 'original_row' is absent
}
y <- data$y  # Store the target variable separately

# Ensure all columns in X are numeric for SMOTE (apply one-hot encoding if necessary)
# SMOTE requires numeric data, so categorical variables are converted into dummy variables
non_numeric_columns <- sapply(X, function(col) !is.numeric(col) && !is.integer(col))
if (any(non_numeric_columns)) {
  X <- dummy_cols(X, select_columns = names(X)[non_numeric_columns], remove_selected_columns = TRUE)
}
X <- as.data.frame(lapply(X, as.numeric))  # Ensure all columns are numeric

#  Apply SMOTE
# Apply SMOTE to create a balanced dataset
smote_result <- SMOTE(X, y, K = 5, dup_size = 2)  # K = number of nearest neighbors, dup_size = oversampling ratio
balanced_data <- smote_result$data  # Extract the balanced dataset
balanced_data$y <- rep(y, length.out = nrow(balanced_data))  # Add back the target variable 'y' to the balanced data
balanced_data$y <- as.factor(balanced_data$y)  # Ensure 'y' is a factor for classification
balanced_data<-balanced_data %>% select(-predicted_probability)

# Train the Random Forest Model
# Exclude non-predictive columns like 'PersonACCId', 'class', or 'index_id'
person_ids <- balanced_data %>% select(PersonACCId)  # Retain 'PersonACCId' separately
model_data <- balanced_data %>%
  select(-y, -class, -PersonACCId)  # Features used for modeling
rf_model_balanced <- randomForest(x = model_data, y = balanced_data$y, ntree = 100)  # Train the Random Forest model

# Evaluate the SMOTE Random Forest Model 
# Predict probabilities and classes for the balanced dataset
pred_probs <- predict(rf_model_balanced, model_data, type = "prob")  # Probabilities for each class
pred_class <- predict(rf_model_balanced, model_data, type = "response")  # Predicted class labels

# Combine predictions with the original dataset for review
predictions_df <- data.frame(
  PersonACCId = person_ids$PersonACCId,
  predicted_prob_class_1 = pred_probs[, 2],  # Predicted probability for the "Yes" class
  predicted_class = pred_class
)
final_predictions <- predictions_df %>%
  right_join(data, by = "PersonACCId")  # Merge predictions back to the original dataset



