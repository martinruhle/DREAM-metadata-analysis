# Load necessary libraries
library(igraph)
library(ggplot2)
library(dplyr)

# Load and prepare data
metadata <- read.csv("C:/Users/User/Documents/DREAM/StudyFiles/Training/Training/metadata/metadata.csv")
rel_abun <- read.csv("C:/Users/User/Documents/DREAM/StudyFiles/Training/Training/taxonomy/taxonomy_relabd.species.csv")

# Combine metadata and abundance data
rel_abun_etn <- metadata %>%
  select(specimen, was_term, NIH.Racial.Category) %>%  
  inner_join(rel_abun, by = "specimen")

# Split data by NIH.Racial.Category to create a list of datasets for each race
race_datasets <- split(rel_abun_etn[, -c(1:3)], rel_abun_etn$NIH.Racial.Category)

# Define the create_co_occurrence_network function with edge color differentiation
create_co_occurrence_network <- function(data, threshold = 0.5) {
  cor_matrix <- cor(data)
  cor_matrix[abs(cor_matrix) < threshold] <- 0  # Apply threshold for absolute values
  
  network <- graph_from_adjacency_matrix(cor_matrix, weighted = TRUE, mode = "undirected", diag = FALSE)
  E(network)$color <- ifelse(E(network)$weight > 0, "blue", "red")
  E(network)$width <- abs(E(network)$weight) * 2
  
  return(network)
}

# Generate networks for preterm and term samples
preterm_data <- rel_abun_etn %>% filter(was_term == "TRUE") %>% select(-specimen, -was_term, -NIH.Racial.Category)
term_data <- rel_abun_etn %>% filter(was_term == "FALSE") %>% select(-specimen, -was_term, -NIH.Racial.Category)

create_co_occurrence_network <- function(data, threshold = 0.5) {
  cor_matrix <- cor(data, use = "pairwise.complete.obs")
  cor_matrix[is.na(cor_matrix)] <- 0
  
  cor_matrix[abs(cor_matrix) < threshold] <- 0
  
  network <- graph_from_adjacency_matrix(cor_matrix, weighted = TRUE, mode = "undirected", diag = FALSE)
  
  # Define edge colors and widths based on the sign of the weight
  E(network)$color <- ifelse(E(network)$weight > 0, "blue", "red")
  E(network)$width <- abs(E(network)$weight) * 2
  
  return(network)
}

network_analysis <- function(network) {
  pos_network <- delete_edges(network, E(network)[weight < 0])
  neg_network <- delete_edges(network, E(network)[weight > 0])
  
  metrics <- list(
    density = edge_density(network),
    transitivity = transitivity(network, type = "global"),
    mean_distance_pos = if (gsize(pos_network) > 0) tryCatch(mean_distance(pos_network, directed = FALSE), error = function(e) NA),
    mean_distance_neg = if (gsize(neg_network) > 0) tryCatch(mean_distance(neg_network, directed = FALSE), error = function(e) NA),
    modularity = modularity(cluster_fast_greedy(network), weights = E(network)$weight)
  )
  
  return(metrics)
}

# Re-create networks and calculate metrics
preterm_network <- create_co_occurrence_network(preterm_data)
term_network <- create_co_occurrence_network(term_data)
race_networks <- lapply(race_datasets, create_co_occurrence_network)

preterm_metrics <- network_analysis(preterm_network)
term_metrics <- network_analysis(term_network)
race_metrics <- lapply(race_networks, network_analysis)

# Print results to verify
print("Preterm Metrics:")
print(preterm_metrics)
print("Term Metrics:")
print(term_metrics)
print("Racial Category Metrics:")
print(race_metrics)




# Load necessary libraries for plotting
library(ggraph)
library(ggplot2)

# Function to plot the network
plot_network <- function(network, title) {
  ggraph(network, layout = "fr") +  # Use Fruchterman-Reingold layout
    geom_edge_link(aes(edge_alpha = abs(weight), edge_color = weight > 0), show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    theme_void() +  # Remove axes
    ggtitle(title) +
    scale_edge_color_manual(values = c("red", "blue"), labels = c("Negative", "Positive"))
}

# Plot the preterm network
plot_network(preterm_network, "Preterm Co-occurrence Network")

# Plot the term network
plot_network(term_network, "Term Co-occurrence Network")

