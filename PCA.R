library(ggplot2)
library(dplyr)
library(FactoMineR)

metadata <- read.csv("C:/Users/User/Documents/DREAM/StudyFiles/Training/Training/metadata/metadata.csv")
rel_abun <- read.csv("C:/Users/User/Documents/DREAM/StudyFiles/Training/Training/taxonomy/taxonomy_relabd.species.csv")

# Join the datasets, and select only the necessary columns
rel_abun_etn <- metadata %>%
  select(specimen, NIH.Racial.Category) %>%  # Select only the necessary columns
  inner_join(rel_abun, by = "specimen")

# Prepare data for PCA
# Exclude the specimen and NIH.Racial.Category columns, keeping only the numeric columns for PCA
pca_data <- rel_abun_etn %>%
  select(-specimen, -NIH.Racial.Category)

# Remove columns with zero variance
pca_data <- pca_data[, sapply(pca_data, 
                              function(col) var(col, na.rm = TRUE) > 0)]

# Normalize the data (z-score normalization)
pca_data_normalized <- scale(pca_data)

# Perform PCA
pca_result <- prcomp(pca_data, scale. = F)
pca_result_norm <- prcomp(pca_data_normalized, scale. = T)

# Get the PCA scores for the first two principal components
pca_scores <- as.data.frame(pca_result$x[, 1:2])
pca_scores_norm <- as.data.frame(pca_result_norm$x[, 1:2])

# Add back the NIH.Racial.Category column to the PCA scores for plotting
pca_scores$NIH.Racial.Category <- rel_abun_etn$NIH.Racial.Category
pca_scores_norm$NIH.Racial.Category <- rel_abun_etn$NIH.Racial.Category

# Plot the PCA with ggplot2
pca_plot <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = NIH.Racial.Category)) +
  geom_point() +
  labs(title = "PCA of Relative Abundance Data",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()

pca_plot_norm <- ggplot(pca_scores_norm, aes(x = PC1, y = PC2, color = NIH.Racial.Category)) +
  geom_point() +
  labs(title = "PCA of Relative Abundance Data",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()

# Print the PCA plot
print(pca_plot)
print(pca_plot_norm)
