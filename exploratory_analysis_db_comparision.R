# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the data
metadata <- read.csv("C:/Users/User/Documents/DREAM/StudyFiles/Training/Training/metadata/metadata.csv")
metadata_norm <- read.csv("C:/Users/User/Documents/DREAM/StudyFiles/Training/Training/metadata/metadata_normalized.csv")

# Basic Statistics and Structure
summary(metadata)
summary(metadata_norm)
str(metadata)
str(metadata_norm)

# Checking for missing values
colSums(is.na(metadata))
colSums(is.na(metadata_norm))

# Compare the number of unique participants in each dataset
unique_participants_metadata <- length(unique(metadata$participant_id))
unique_participants_metadata_norm <- length(unique(metadata_norm$participant_id))

cat("Unique Participants in metadata:", unique_participants_metadata, "\n")
cat("Unique Participants in metadata_norm:", unique_participants_metadata_norm, "\n")

# Distribution of Samples per Participant in each dataset
samples_per_participant_metadata <- metadata %>% 
  group_by(participant_id) %>% 
  summarize(sample_count = n())

samples_per_participant_metadata_norm <- metadata_norm %>% 
  group_by(participant_id) %>% 
  summarize(sample_count = n())

# Plotting Samples per Participant
ggplot(samples_per_participant_metadata, aes(x = sample_count)) + 
  geom_histogram(binwidth = 1, fill = "#ca684d") + 
  xlab("Number of Samples per Participant") + 
  ylab("Count") + 
  ggtitle("Samples per Participant in metadata")

ggplot(samples_per_participant_metadata_norm, aes(x = sample_count)) + 
  geom_histogram(binwidth = 1, fill = "#7aa364") + 
  xlab("Number of Samples per Participant") + 
  ylab("Count") + 
  ggtitle("Samples per Participant in metadata_norm")

# Race and Ethnicity Sample Distribution in metadata
race_distribution_metadata <- table(metadata$race)
ethnicity_distribution_metadata <- table(metadata$`NIH Racial Category`)

# Race and Ethnicity Distribution in metadata_norm
# Assuming race and ethnicity are one-hot encoded in metadata_norm
metadata_norm_long <- metadata_norm %>%
  pivot_longer(cols = starts_with("Race"), names_to = "race", values_to = "count") %>%
  filter(count == 1) %>%
  select(-count)

race_distribution_metadata_norm <- table(metadata_norm_long$race)

# Plotting Sample Race Distribution
ggplot(metadata, aes(x = race)) + 
  geom_bar() + 
  xlab("Race") + 
  ylab("Count") + 
  ggtitle("Distribution of Sample Race in metadata")

ggplot(metadata_norm_long, aes(x = race)) + 
  geom_bar() + 
  xlab("Race") + 
  ylab("Count") + 
  ggtitle("Distribution of Sample Race in metadata_norm")

# Correlation Analysis in metadata

# Slice metadata to only one row per participant
metadata_slice <- metadata %>%
  group_by(participant_id) %>%
  slice_max(collect_wk, with_ties = FALSE) %>%
  ungroup()
# Convert factors to numeric for correlation analysis if necessary
metadata_slice <- metadata_slice %>%
  mutate(
    age = if_else(age == "Unknown", 0, as.numeric(as.character(age)))
  )
metadata_slice <- metadata_slice %>%
  mutate(across(c(delivery_wk, was_term, was_preterm, was_early_preterm, race, NIH.Racial.Category, NIH.Ethnicity.Category),
                ~ as.numeric(as.factor(.)),
                .names = "numeric_{col}"))

correlation_matrix_metadata_slice <- cor(metadata_slice[, c("age", "numeric_delivery_wk", "race_numeric", "nih_racial_category_numeric", "nih_ethnicity_category_numeric", "numeric_was_term", "numeric_was_preterm", "numeric_was_early_preterm")], use = "complete.obs")
print(correlation_matrix_metadata_slice)

# Correlation Analysis in metadata_norm
metadata_norm$age_numeric <- as.numeric(as.character(metadata_norm$age))
# Converting one-hot encoded race to numeric for correlation
race_columns <- grep("^Race: ", colnames(metadata_norm), value = TRUE)
ethnicity_columns <- grep("^Ethnicity: ", colnames(metadata_norm), value = TRUE)

correlation_matrix_metadata_norm <- cor(metadata_norm[, c(race_columns, ethnicity_columns, "age_numeric", "collect_wk")], use = "complete.obs")
print(correlation_matrix_metadata_norm)