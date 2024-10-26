# Load necessary libraries
library(pheatmap)
library(dplyr)

# Read the metadata and relative abundance files
metadata <- read.csv("C:/Users/User/Documents/DREAM/StudyFiles/Training/Training/metadata/metadata.csv")
rel_abun <- read.csv("C:/Users/User/Documents/DREAM/StudyFiles/Training/Training/taxonomy/taxonomy_relabd.species.csv")

# Join the datasets, and select only the necessary columns
rel_abun_etn <- metadata %>%
  select(specimen, was_term, NIH.Racial.Category) %>%  # Select only the necessary columns
  inner_join(rel_abun, by = "specimen")

# Step 1: Filter variables by variance (top 500 most variable bacteria)
bacteria_data <- rel_abun_etn[, 4:2121]  # Bacteria data (columns 3 to 2120)
metadata <- rel_abun_etn[, 1:3]  # Metadata (columns 1 to 3)

var_threshold <- 50  # Number of variables to keep (for illustration purposes)
var_filtered <- bacteria_data %>%
  select(order(apply(., 2, var), decreasing = TRUE)[1:var_threshold])

# Step 2: Check metadata and handle any missing values
metadata$NIH.Racial.Category <- as.factor(metadata$NIH.Racial.Category)
metadata$was_term <- as.factor(metadata$was_term)

if (any(is.na(metadata$NIH.Racial.Category) | is.na(metadata$was_term))) {
  stop("There are missing values in NIH.Racial.Category or was_term")
}

# Step 3: Verify that row names are correctly aligned
rownames(var_filtered) <- rownames(metadata) <- 1:nrow(var_filtered)

# Step 4: Prepare the combined annotation for the heatmap
annotation_col <- data.frame(Race = metadata$NIH.Racial.Category,
                             Was_term = metadata$was_term)
rownames(annotation_col) <- rownames(var_filtered)

# Check the levels of the categories
print(levels(metadata$NIH.Racial.Category))
print(levels(metadata$was_term))

# Step 5: Define colors for both categories
# Create a custom color palette for the race and was_term annotations
race_colors <- RColorBrewer::brewer.pal(n = length(levels(metadata$NIH.Racial.Category)), 
                                        name = "Set3")  # Or use any other palette
term_colors <- c("FALSE" = "red", "TRUE" = "blue")

# Create the annotation_colors list for pheatmap
annotation_colors <- list(Race = setNames(race_colors, levels(metadata$NIH.Racial.Category)),
                          Was_term = term_colors)

# Step 6: Transpose the bacteria data so specimens are the columns and bacteria are the rows
var_filtered_t <- t(var_filtered)

# Prepare the combined annotation for the heatmap (both Race and Was_term)
annotation_col <- data.frame(Race = metadata$NIH.Racial.Category,
                             Was_term = metadata$was_term)
rownames(annotation_col) <- rownames(var_filtered)

# Reorder specimens (columns) based on NIH.Racial.Category
ordered_indices <- order(metadata$NIH.Racial.Category)
var_filtered_t_ordered <- var_filtered_t[, ordered_indices]
annotation_col_ordered <- annotation_col[ordered_indices, , drop=FALSE]

# Save the heatmap with higher quality and no scaling (use original relative abundances)
png(filename = "heatmap_by_race_term.png", 
    width = 6000, height = 4000, res = 300)  # High resolution (300 DPI)

pheatmap(var_filtered_t_ordered, 
         annotation_col = annotation_col_ordered,  # Ordered annotation for both Race and Was_term
         annotation_colors = annotation_colors,    # Color scheme for NIH.Racial.Category and Was_term
         clustering_method = "complete",           # Cluster only bacteria
         cluster_cols = TRUE,                      # Cluster specimens
         scale = "none",                           # No scaling, keep original relative abundances
         show_rownames = FALSE, 
         show_colnames = FALSE,
         main = "Heatmap of Bacteria Abundances Grouped by NIH.Racial.Category and Was_term")
dev.off()  # Close the PNG device
