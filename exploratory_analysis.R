# Load necessary libraries
library(ggplot2)
library(dplyr)
library(polycor)

# Read the data
metadata <- read.csv("C:/Users/User/Documents/DREAM/StudyFiles/Training/Training/metadata/metadata.csv")

# Basic Statistics and Structure
summary(metadata)
str(metadata)

# Checking for missing values
colSums(is.na(metadata))

# Creating a dataset with one row per participant
participant_data <- metadata %>%
  group_by(participant_id) %>%
  summarize(
    age = as.numeric(first(na.omit(age))),
    race = names(sort(table(race), decreasing = TRUE))[1],
    nih_racial_category = names(sort(table(`NIH.Racial.Category`), decreasing = TRUE))[1],
    nih_ethnicity_category = names(sort(table(`NIH.Ethnicity.Category`), decreasing = TRUE))[1],
    was_term = any(was_term == TRUE),
    was_preterm = any(was_preterm == TRUE),
    was_early_preterm = any(was_early_preterm == TRUE),
    delivery_wk = min(delivery_wk, na.rm = TRUE),
    collect_wk_earliest = min(collect_wk, na.rm = TRUE),
    collect_wk_latest = max(collect_wk, na.rm = TRUE),
    n_samples = n()
  )

# Basic Statistics for Participant-level Data
summary(participant_data)
str(participant_data)

# Demographic Analysis
# Race/Ethnicity Distribution
ethnicity_distribution <- table(participant_data$nih_racial_category)

# Perform chi-squared test
chi_sq_test <- chisq.test(table(participant_data$nih_racial_category, participant_data$was_term))

# Extract the p-value
p_value <- chi_sq_test$p.value

# Calculate the count and percentage for each combination of nih_racial_category and was_term
data_summary <- participant_data %>%
  group_by(nih_racial_category, was_term) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(nih_racial_category) %>%
  mutate(total_count = sum(count),
         percentage = count / total_count * 100) %>%
  filter(was_term == FALSE)  # Keep only rows where was_term is FALSE

# Order the levels of nih_racial_category by the percentage of FALSE
ordered_categories <- data_summary %>%
  arrange(percentage) %>%
  pull(nih_racial_category) %>%
  unique()

# Update participant_data with ordered categories
participant_data$nih_racial_category <- factor(participant_data$nih_racial_category, levels = ordered_categories)

# Create the plot
ggplot(participant_data, aes(x = nih_racial_category, fill = was_term)) + 
  geom_bar(position = "stack") + 
  geom_text(data = data_summary, 
            aes(y = count, label = sprintf("%.1f%%", percentage)), 
            vjust = -4) +
  xlab("NIH Racial Category") + 
  ylab("Count") + 
  ggtitle("Distribution of NIH Racial Category")+ 
  annotate("text", x = Inf, y = Inf, label = paste("chi_sq p-value:", format(p_value, digits = 2)), hjust = 1.1, vjust = 2, size = 3)+
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# Convert the age column to a factor including NA's as a separate level
participant_data$age_factor <- factor(participant_data$age, exclude = NULL)

# Age Participant Distribution with NA's as a separate bin
ggplot(participant_data, aes(x = age_factor)) + 
  geom_histogram(stat = "count", fill = "#5ea758", color = "grey") + 
  xlab("Age") + 
  ylab("Count") + 
  ggtitle("Age Participant Distribution")

# Convert logical columns to numeric
participant_data$was_preterm_num <- as.numeric(participant_data$was_preterm)
participant_data$was_early_preterm_num <- as.numeric(participant_data$was_early_preterm)

# Calculate the correlations
age_preterm_cor <- polyserial(participant_data$age, participant_data$was_preterm_num)
age_early_preterm_cor <- polyserial(participant_data$age, participant_data$was_early_preterm_num)
preterm_early_preterm_cor <- polychor(participant_data$was_preterm_num, participant_data$was_early_preterm_num)

# Calculate the p-values
age_preterm_test <- cor.test(participant_data$age, participant_data$was_preterm_num, method = "pearson")
age_early_preterm_test <- cor.test(participant_data$age, participant_data$was_early_preterm_num, method = "pearson")
preterm_early_preterm_test <- cor.test(participant_data$was_preterm_num, participant_data$was_early_preterm_num, method = "pearson")

# Create a formatted matrix with correlations and significance levels
formatted_matrix <- matrix(NA, nrow = 3, ncol = 3)
formatted_matrix[1, 1] <- "1.00"
formatted_matrix[1, 2] <- paste0(round(age_preterm_cor, 2), " (p=", format(age_preterm_test$p.value, digits = 2), ")")
formatted_matrix[1, 3] <- paste0(round(age_early_preterm_cor, 2), " (p=", format(age_early_preterm_test$p.value, digits = 2), ")")
formatted_matrix[2, 1] <- paste0(round(age_preterm_cor, 2), " (p=", format(age_preterm_test$p.value, digits = 2), ")")
formatted_matrix[2, 2] <- "1.00"
formatted_matrix[2, 3] <- paste0(round(preterm_early_preterm_cor, 2), " (p=", format(preterm_early_preterm_test$p.value, digits = 2), ")")
formatted_matrix[3, 1] <- paste0(round(age_early_preterm_cor, 2), " (p=", format(age_early_preterm_test$p.value, digits = 2), ")")
formatted_matrix[3, 2] <- paste0(round(preterm_early_preterm_cor, 2), " (p=", format(preterm_early_preterm_test$p.value, digits = 2), ")")
formatted_matrix[3, 3] <- "1.00"

# Overlaid Age Participant Distribution by was_term
ggplot(participant_data, aes(x = age_factor, fill = as.factor(was_term))) + 
  geom_histogram(stat = "count", position = "identity", alpha = 0.5, color = "black") + 
  xlab("Age") + 
  ylab("Count") + 
  ggtitle("Age Participant Distribution by Term Status") +
  scale_fill_manual(name = "Term Status", values = c("TRUE" = "blue", "FALSE" = "red"), labels = c("TRUE" = "Term", "FALSE" = "Preterm")) + 
  annotate("text", x = Inf, y = Inf, label = paste("point-biserial corr:", format(formatted_matrix[1,2], digits = 2)), hjust = 1.1, vjust = 2, size = 3)


# Pregnancy and Delivery Details
# Term Status
# Assuming participant_data is your dataframe with columns was_term, was_preterm, and was_early_preterm

# Create the table
term_status <- table(participant_data$was_term, participant_data$was_preterm, participant_data$was_early_preterm)

# Add descriptive names to the dimensions
dimnames(term_status) <- list(
  Term_Status = c("Preterm", "Full-Term"),
  Preterm_Status = c("Full-Term", "Preterm"),
  Early_Preterm_Status = c("Not Early Preterm", "Early Preterm")
)

# Display the table with descriptive names
print(term_status)

# Calculate percentages
total_births <- sum(term_status)
preterm_births <- sum(term_status[, "Preterm", ])
early_preterm_births <- sum(term_status[, , "Early Preterm"])

preterm_percentage <- (preterm_births / total_births) * 100
early_preterm_percentage <- (early_preterm_births / total_births) * 100

# Print the percentages
cat(sprintf("Percentage of Preterm Births: %.2f%%\n", preterm_percentage))
cat(sprintf("Percentage of Early Preterm Births: %.2f%%\n", early_preterm_percentage))

# Plotting Delivery Week Distribution
ggplot(participant_data, aes(x = delivery_wk, fill = as.factor(was_term))) + 
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5, color = "black") + 
  xlab("Delivery Week") + 
  ylab("Count") + 
  ggtitle("Distribution of Delivery Week") +
  scale_fill_manual(name = "Term Status", values = c("TRUE" = "blue", "FALSE" = "red"), labels = c("TRUE" = "Term", "FALSE" = "Preterm"))

# Plotting sample collection weeks
ggplot(metadata, aes(x = collect_wk)) + 
  geom_histogram(binwidth = 1, fill = "#5ea758", color = "grey") + 
  xlab("Collection Week") + 
  ylab("Count") + 
  ggtitle("Distribution of Sample Collection Week")

# Categorize collection weeks
participant_data <- participant_data %>%
  mutate(
    collect_wk_category = case_when(
      collect_wk_earliest == collect_wk_latest ~ "single",
      TRUE ~ "multiple"
    )
  )

# Separate data for multiple and single categories
multiple_data <- participant_data %>% filter(collect_wk_category == "multiple")
single_data <- participant_data %>% filter(collect_wk_category == "single")

# Plotting Collection Week Distribution (Earliest and Latest)
ggplot() + 
  geom_histogram(data = multiple_data, aes(x = collect_wk_earliest, fill = "Earliest"), binwidth = 1, color = "grey", alpha = 0.5, position = "identity") + 
  geom_histogram(data = multiple_data, aes(x = collect_wk_latest, fill = "Latest"), binwidth = 1, color = "grey", alpha = 0.5, position = "identity") + 
  geom_histogram(data = single_data, aes(x = collect_wk_earliest, fill = "single"), binwidth = 1, color = "grey", alpha = 0.5, position = "identity") + 
  xlab("Collection Week") + 
  ylab("Count") + 
  ggtitle("Collection Week") +
  scale_fill_manual(name = "Collection Week Category", values = c("Earliest" = "blue", "Latest" = "red", "single" = "green"), 
                    labels = c("Earliest", "Latest", "Single")) +
  theme(legend.position = "right")

# Distribution of Number of Samples per Participant
ggplot(participant_data, aes(x = n_samples)) + 
  geom_histogram(binwidth = 1, fill = "#5ea758", color = "grey") + 
  xlab("Number of Samples per Participant") + 
  ylab("Count") + 
  ggtitle("Distribution of Number of Samples per Participant")
