# -------------------------- Script Information --------------------------
# Purpose:
# This script evaluates and interprets the performance of two Random Forest models (Model 1 and Model 2) 
# Key steps include diagnostics, feature importance analysis, and visualization to understand and improve model effectiveness.

# Benefits:
# - AUC provides a clear metric for comparing the effectiveness of the two models.
# - Variable importance analysis highlights the most influential predictors, enabling data-driven 
#   decision-making and feature selection.

# Considerations:
# - AUC and ROC curves should be interpreted in conjunction with other performance metrics, such as 
#   precision, recall, and F1-score, to fully assess the model's effectiveness.
# - Feature importance rankings are model-specific and may vary depending on the algorithm and data 
#   preprocessing steps.
# - Ensure that data used in evaluations (e.g., `pred_probs`, `balanced_data`) matches the appropriate 
#   model training and testing subsets to avoid data leakage.

# Dependencies:
# - Inputs: Cleaned dataset (`data`), Model 1 (`rf_model`), and Model 2 (`rf_model_balanced`).
# - Outputs: AUC values, sample predictions, and visualizations of feature importance and ROC curves.
# ------------------------------------------------------------------------


# Model 1 diagnostics
model_data <- data %>%
  select(PersonACCId,age_at_extraction_date, ethnicity_last_claim, location_tla_last_claim, work_last_claim,
         Areaunit_score, num_gym_all, num_wgt_all, total_injuries, y) %>%
  mutate(y = as.factor(y)) %>%  # Ensure target is a factor (binary classification)
  drop_na()  # Drop rows with missing values to avoid issues in model evaluation

roc_curve <- roc(model_data$y, pred_prob)  # Calculate ROC curve for Model 1
auc_model1 <- auc(roc_curve)  # Calculate the Area Under the Curve (AUC) for Model 1

# Print diagnostics for Model 1
print(rf_model)  # Print the random forest model summary for Model 1
cat("AUC for Model 1:", auc_model1, "\n")  # Print the AUC value to assess model performance

# Add predicted probabilities to the data for further analysis
data$predicted_probability <- pred_prob

# Variable Importance
# Extract and display the variable importance scores from the Random Forest model
variable_importance <- importance(rf_model)
print(variable_importance)  # Print variable importance to understand which predictors are most important

# Visualize variable importance 
importance_df <- data.frame(Variable = rownames(variable_importance), Importance = variable_importance[, 1])

top_5_vars_rf <- importance_df[1:5, ]  # Select the top 6 features

# Create a bar plot to visualize the importance of each variable
plot1 <- ggplot(top_5_vars_rf, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Bar plot of variable importance
  coord_flip() +  # Flip the axes for better readability
  theme_minimal() +  # Minimal theme for better visual appeal
  labs(title = "Baseline Random Forest", x = "Variables", y = "Importance")  # Add titles and labels

# Plot ROC curve for Model 1
plot2 <-ggroc(roc_curve) +  # Plot ROC curve for Model 1
  labs(title = paste("ROC Curve - AUC =", round(auc_model1, 3)),  # Add title with AUC value
       x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)") +  # Label axes
  theme_minimal()  # Apply minimal theme for a clean plot

# Model 2 diagnostics
# Analyze Feature Importance ---
# Extract variable importance scores from Model 2 (Post-SMOTE)
var_imp <- importance(rf_model_balanced)  # Get feature importance from SMOTE-balanced model
var_imp_df <- as.data.frame(var_imp)  # Convert importance scores to data frame
var_imp_df$Variable <- rownames(var_imp_df)  # Add variable names as a column
var_imp_df <- var_imp_df %>% arrange(desc(MeanDecreaseGini))  # Sort by the importance scores in descending order

# Plot the top 5 most important features based on Mean Decrease in Gini
top_5_vars <- var_imp_df[1:5, ]  # Select the top 6 features

plot3 <- ggplot(top_5_vars, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Bar plot of variable importance
  coord_flip() +  # Flip the axes for better readability
  theme_minimal() +  # Minimal theme for better visual appeal
  labs(title = "SMOTE Random Forest", x = "Variables", y = "Importance")  # Add titles and labels


# Calculate ROC curve and AUC for Model 2 (Post-SMOTE)
roc_curve_smote <- roc(balanced_data$y, pred_probs[, 2])  # ROC curve for Model 2, using probabilities for "Yes" class
auc_value_smote <- auc(roc_curve_smote)  # Calculate AUC for Model 2

# Print AUC value for Model 2
print(paste("AUC for the Random Forest Model after SMOTE: ", auc_value_smote))

# Plot ROC curve for Model 2 (Post-SMOTE)
plot4 <- ggroc(roc_curve_smote) +  # Plot ROC curve for Model 2
  labs(title = paste("ROC Curve - AUC =", round(auc_value_smote, 3)),  # Add title with AUC value
       x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)") +  # Label axes
  theme_minimal()  # Apply minimal theme for a clean plot


# # Model 3 diagnostics
# # Calculate AUC
# roc_curve_xg <- roc(y, pred_prob)  # ROC curve
# auc_xgb <- auc(roc_curve_xg)  # Area under the curve
# 
# # Print diagnostics for XGBoost
# cat("AUC for XGBoost Model:", auc_xgb, "\n")
# 
# # Add predicted probabilities to the data
# data$predicted_probability <- pred_prob
# 
# # --- Step 7: Variable Importance ---
# # Get and display variable importance
# variable_importance_xg <- xgb.importance(model = xgb_model)
# print(variable_importance_xg)
# 
# # Visualize variable importance
# # Get top 5 most important features
# top5_variable_importance <- variable_importance_xg %>%
#   top_n(5, Gain)
# 
# # Plot the top 5 most important features
# ggplot(top5_variable_importance, aes(x = reorder(Feature, Gain), y = Gain)) +
#   geom_bar(stat = "identity", fill = "steelblue") +
#   coord_flip() +
#   theme_minimal() +
#   labs(title = "Variable Importance  XGBoost", x = "Features", y = "Gain")
# 
# 
# 
# # --- Generate Predicted Class Labels ---
# # Apply a threshold of 0.5 to the predicted probabilities to get class labels (0 or 1)
# pred_class <- ifelse(pred_prob > 0.5, 1, 0)
# 
# # --- Confusion Matrix ---
# # Create confusion matrix using caret package
# conf_matrix <- confusionMatrix(factor(pred_class), factor(y))
# 
# # Print confusion matrix
# print(conf_matrix)
# 
