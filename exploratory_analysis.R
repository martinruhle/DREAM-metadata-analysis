# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the data
metadata <- read.csv("C:/Users/User/Documents/DREAM/StudyFiles/Training/Training/metadata/metadata.csv")

# Basic Statistics
summary(metadata)

# Checking data types and missing values
str(metadata)
colSums(is.na(metadata))

# Demographic Analysis
# Race and Ethnicity Distribution
race_distribution <- table(metadata$race)
ethnicity_distribution <- table(metadata$`NIH Racial Category`)

# Plotting Race Distribution
ggplot(metadata, aes(x = race)) + 
  geom_bar() + 
  xlab("Race") + 
  ylab("Count") + 
  ggtitle("Distribution of Race")

# Age Distribution
ggplot(metadata, aes(x = age)) + 
  geom_bar(binwidth = 5) + 
  xlab("Age") + 
  ylab("Count") + 
  ggtitle("Age Distribution")

# Pregnancy and Delivery Details
# Term Status
term_status <- table(metadata$was_term, metadata$was_preterm, metadata$was_early_preterm)

# Plotting Delivery Week Distribution
ggplot(metadata, aes(x = delivery_wk)) + 
  geom_histogram(binwidth = 1) + 
  xlab("Delivery Week") + 
  ylab("Count") + 
  ggtitle("Distribution of Delivery Week")

# Unique Participants and Specimens Check
unique_participants <- length(unique(metadata$participant_id))
unique_specimens <- length(unique(metadata$specimen))

# Cross-checking Consistency
# Example: Checking if all 'was_early_preterm' are also 'was_preterm'
consistency_check <- all(metadata$was_early_preterm == TRUE & metadata$was_preterm == TRUE)

