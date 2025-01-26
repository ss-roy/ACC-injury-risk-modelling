# -------------------------- Script Information --------------------------
# Purpose:
# This script generates a series of visualizations to provide the Programme Manager 
# with a clear understanding of the final population selected for gym injury prediction. 
# The visualizations highlight key characteristics of the sample, including the distribution 
# of age bins, ethnicity, predicted probabilities, and how these variables interact with one another. 
# This will help ensure that the sampling process is transparent, equitable, and aligned with the project’s objectives.

# Benefits:
# - Provides transparency on the diversity and equity of the final sample used for analysis.
# - Ensures that the Programme Manager can evaluate the sampling method based on demographic distributions.
# - Assesses how predicted injury probabilities vary across key demographic factors like age and ethnicity.

# Considerations:
# - The visualizations will only reflect the stratified sample selected with high predicted probabilities 
#   (i.e., above 0.50). This ensures the sample is focused on individuals at higher risk for gym-related injuries.
# - The age bins and ethnicity categories should be checked to ensure they align with the program’s intended analysis.
# - Visualizations involving ethnicity may need to be interpreted with consideration of cultural and demographic contexts.

# Dependencies:
# - Inputs: A dataset (`stratified_sample`) containing the age, ethnicity, predicted probabilities 
#   of injury, and other relevant demographic features.
# - Outputs: Multiple visualizations (bar plots, histograms, boxplots) that provide insights into 
#   the demographic distribution and predicted injury probabilities within the sample.
# ------------------------------------------------------------------------

stratified_sample<-read.csv("out/ACC_intervention_list_500.csv")
top_500 <- read.csv("out/top_500.csv")
stratified_sample %>% filter(age_at_extraction_date>14 & age_at_extraction_date<75)

# Age bin distribution
plot5<-ggplot(stratified_sample, aes(x = age_bin)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Age Bins in the Sample",
       x = "Age Bin",
       y = "Count") +
  theme_minimal()

# Ethnicity distribution
plot6<-ggplot(stratified_sample, aes(x = ethnicity_last_claim)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Ethnicity in the Sample",
       x = "Ethnicity",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability

# Predicted probabilities distribution
plot7<-ggplot(stratified_sample, aes(x = predicted_prob_class_1)) +
  geom_histogram(binwidth = 0.05, fill = "lightcoral", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Predicted Probabilities",
       x = "Predicted Probability of Gym Injury",
       y = "Count") +
  theme_minimal()

# Ethnicity vs Predicted Probability
plot8<-ggplot(stratified_sample, aes(x = ethnicity_last_claim, y = predicted_prob_class_1)) +
  geom_boxplot(aes(color = ethnicity_last_claim), alpha = 0.7) +
  labs(title = "Ethnicity vs Predicted Probability of Injury",
       x = "Ethnicity",
       y = "Predicted Probability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability

# Proportions of age bins and ethnicity
table_age_ethnicity <- stratified_sample %>%
  group_by(age_bin, ethnicity_last_claim) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

plot9<-ggplot(table_age_ethnicity, aes(x = age_bin, y = proportion, fill = ethnicity_last_claim)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Proportions of Age and Ethnicity in the Sample",
       x = "Age Bin",
       y = "Proportion",
       fill = "Ethnicity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability

# Count of records in each age and ethnicity bin
plot10<-stratified_sample %>%
  group_by(age_bin, ethnicity_last_claim) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = age_bin, y = count, fill = ethnicity_last_claim)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count of Records per Age and Ethnicity Bin",
       x = "Age Bin",
       y = "Count",
       fill = "Ethnicity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability


# Comparing top 500 selection vs stratified selection ---------------------

# Add an identifier column to both data frames
top_500$sample_type <- "Top 500"
stratified_sample$sample_type <- "Stratified Sample"

# Combine data frames for age comparison
combined_data <- rbind(
  top_500[, c("age_at_extraction_date", "sample_type")],
  stratified_sample[, c("age_at_extraction_date", "sample_type")]
)

# Combine data frames for ethnicity comparison
ethnicity_data <- rbind(
  top_500[, c("ethnicity_last_claim", "sample_type")],
  stratified_sample[, c("ethnicity_last_claim", "sample_type")]
)

# Density plot for age distribution
p1 <- ggplot(combined_data, aes(x = age_at_extraction_date, fill = sample_type)) +
  geom_density(alpha = 0.6) +
  labs(title = "Age Distribution Comparison",
       x = "Age",
       y = "Density",
       fill = "Sample Type") +
  theme_minimal()

# Bar plot for ethnicity distribution
p2 <- ggplot(ethnicity_data, aes(x = ethnicity_last_claim, fill = sample_type)) +
  geom_bar(position = "dodge") +
  labs(title = "Ethnicity Distribution Comparison",
       x = "Ethnicity",
       y = "Count",
       fill = "Sample Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Proportion of ethnicities across samples
ethnicity_data_proportion <- ethnicity_data %>%
  group_by(sample_type, ethnicity_last_claim) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(proportion = count / sum(count))

p3 <- ggplot(ethnicity_data_proportion, aes(x = sample_type, y = proportion, fill = ethnicity_last_claim)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Proportional Ethnicity Distribution",
       x = "Sample Type",
       y = "Proportion",
       fill = "Ethnicity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Grouped bar chart for age and ethnicity
combined_age_ethnicity <- rbind(
  top_500[, c("age_at_extraction_date", "ethnicity_last_claim", "sample_type")],
  stratified_sample[, c("age_at_extraction_date", "ethnicity_last_claim", "sample_type")]
)

p4 <- ggplot(combined_age_ethnicity, aes(x = ethnicity_last_claim, fill = sample_type)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ cut(age_at_extraction_date, breaks = c(15, 25, 35, 45, 55, 65), 
                   labels = c("15-25", "26-35", "36-45", "46-55", "56+"))) +
  labs(title = "Age vs Ethnicity Breakdown",
       x = "Ethnicity",
       y = "Count",
       fill = "Sample Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display all plots
print(p1) # Age Distribution
print(p2) # Ethnicity Distribution
print(p3) # Proportional Ethnicity Distribution
print(p4) # Age vs Ethnicity Breakdown





