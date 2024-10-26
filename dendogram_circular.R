# Color labels by NIH.Racial.Category
dend_samples <- dend_samples %>%
  set("labels_col", metadata_ordered$NIH.Racial.Category, 
      k = length(unique(metadata_ordered$NIH.Racial.Category))) %>%
  set("labels_cex", 0.7)  # Adjust label size if necessary

# Color vector for NIH.Racial.Category (ensure unique categories match)
race_colors <- setNames(c("red", "blue", "green", "yellow", "orange", "purple"), 
                        levels(metadata_ordered$NIH.Racial.Category))

# Reset circos parameters to defaults, then set custom parameters
circos.clear()  # Clear any previous settings

# Set custom circos parameters
circos.par(
  start.degree = 90,              # Starting point of the plot
  gap.degree = 1,                 # Small uniform gap between all sectors
  track.margin = c(0.01, 0.01),   # Margins around the circular plot
  cell.padding = c(0, 0, 0, 0)    # No padding around the cells
)

# Circular layout for dendrogram without sample names
circlize_dendrogram(dend_samples, 
                    labels_track_height = 0.3,   # Reduced labels track height
                    dend_track_height = 0.3,     # Reduced dendrogram track height
                    labels_gp = gpar(col = NA))  # Hide labels by setting color to NA

# Create a new track for colored bars
circos.track(
  ylim = c(0, 1),                 # Set y limits for the track
  track.height = 0.2,             # Height of the track
  panel.fun = function(x, y) {
    for (i in seq_along(metadata_ordered$NIH.Racial.Category)) {
      circos.rect(
        xleft = i - 0.5, 
        xright = i + 0.5, 
        ybottom = 0, 
        ytop = 1, 
        col = race_colors[metadata_ordered$NIH.Racial.Category[i]],
        border = NA
      )
    }
  }
)

# Reset circos parameters after plotting
circos.clear()

# Add legend for NIH.Racial.Category
legend("topright", legend = levels(metadata_ordered$NIH.Racial.Category), 
       col = race_colors, pch = 15, title = "NIH.Racial.Category")