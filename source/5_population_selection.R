# -------------------------- Script Information --------------------------
# Purpose:
# This script performs stratified sampling to select a diverse and representative pool 
# of 500 individuals for a gym injury prediction model. The sampling process is based on 
# predicted injury probabilities and ensures equity by considering both age and ethnicity 
# in the selection process. The goal is to provide a balanced and fair subset for further analysis.

# Key Steps:
# 1. Top 500 by Predicted Probability:
#    - Sort the data by predicted injury probabilities (`predicted_prob_class_1`) and select 
#      the top 500 individuals with the highest probabilities.
# 2. Stratified Sampling by Age and Ethnicity:
#    - Create age bins and perform stratified sampling based on age and ethnicity, ensuring 
#      diversity across groups.
#    - Sample 15 individuals per combination of age group and ethnicity where the predicted 
#      probability is greater than 0.50.
# 3. Output:
#    - Generate a final sample of 500 individuals, ensuring balanced representation across 
#      key demographics, and print summary statistics of the selected sample.

# Highlights:
# - Ensures diverse and representative sampling based on age and ethnicity, addressing equity 
#   concerns.
# - Utilizes predicted probabilities to focus on individuals at higher risk for injury.
# - The stratified approach avoids over-representation of any particular group, providing a 
#   balanced sample.

# Considerations:
# - Only individuals with predicted probabilities above 0.50 are included in the stratified sample.
# - Stratified sampling may limit the total sample size if some demographic groups do not have enough 
#   individuals with predicted probabilities above 0.50.
# - The process uses a fixed seed (`set.seed(123)`) to ensure reproducibility of the results.

# Dependencies:
# - Inputs: A dataset (`final_predictions`) with columns including age, ethnicity, injury counts, 
#   and predicted probabilities (`predicted_prob_class_1`).
# - Outputs: A stratified sample of 500 individuals (`stratified_sample`) for further analysis or 
#   intervention planning.
# ------------------------------------------------------------------------

final_predictions <- final_predictions %>% select(PersonACCId,age_at_extraction_date,
                                      ethnicity_last_claim,location_tla_last_claim,
                                      work_last_claim,Areaunit_score,num_gym_all, num_wgt_all,
                                      YEJune_last_claim,total_injuries,predicted_prob_class_1)


# top 500
top_500 <- final_predictions %>%
  arrange(desc(final_predictions)) %>%
  slice_head(n = 500)

# Proportional sampling for Areaunit bins (adjusted approach)

# Step 2: Perform Stratified Sampling based on Age and Ethnicity
set.seed(123)  # for reproducibility

# Stratified sampling
# Perform stratified sampling and pick top 500 rows
# stratified_sample$y <-as.factor(stratified_sample$y)

stratified_sample <- final_predictions %>%
  mutate(age_bin = case_when(
    age_at_extraction_date >= 0 & age_at_extraction_date <= 14 ~ "0-14",
    age_at_extraction_date >= 15 & age_at_extraction_date <= 25 ~ "15-25",
    age_at_extraction_date >= 26 & age_at_extraction_date <= 35 ~ "26-35",
    age_at_extraction_date >= 36 & age_at_extraction_date <= 45 ~ "36-45",
    age_at_extraction_date >= 46 & age_at_extraction_date <= 55 ~ "46-55",
    age_at_extraction_date >= 56 ~ "56 and over"
  )) %>%
  group_by(age_bin, ethnicity_last_claim) %>%
  filter(predicted_prob_class_1>0.50) %>% 
  sample_n(17,replace = TRUE) %>%  # Sampling 15 individuals from each combination of age_bin and ethnicity_last_claim
  ungroup() %>%
  slice_head(n = 500)  # Select top 500 rows

# Check the result
head(stratified_sample)

stratified_sample %>% summary()

stratified_sample <-stratified_sample %>% select(PersonACCId,age_at_extraction_date,age_bin,
                                                 ethnicity_last_claim,work_last_claim,
                                                 location_tla_last_claim,Areaunit_score,predicted_prob_class_1)

# Save the final list

write.csv(top_500,"out/top_500.csv",row.names = FALSE)
write.csv(stratified_sample,"out/ACC_intervention_list_500.csv",row.names = FALSE)

