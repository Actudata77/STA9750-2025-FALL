# ============================================================================
# Task 3: Plot All Tree Points
# ============================================================================
# Create a ggplot2 map showing trees as points over council districts

library(ggplot2)
library(sf)
library(dplyr)

# Load data if not already in environment
# DISTRICTS <- readRDS("data/mp03/districts.rds")
# TREES <- readRDS("data/mp03/trees.rds")

# Create the map
cat("Creating map of all trees and districts...\n")

ggplot() +
  # Layer 1: Council district boundaries (polygons)
  geom_sf(data = DISTRICTS, fill = NA, color = "black", linewidth = 0.5) +
  # Layer 2: Tree points
  geom_sf(data = TREES, color = "darkgreen", size = 0.1, alpha = 0.3) +
  labs(
    title = "NYC Street Trees by Council District",
    subtitle = paste0("All ", format(nrow(TREES), big.mark = ","), " trees across 51 council districts"),
    caption = "Data: NYC OpenData"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 8)
  )

cat("✓ Map created!\n\n")

# ============================================================================
# Task 4: District-Level Analysis of Tree Coverage
# ============================================================================

cat("Performing spatial join of trees to districts...\n")

# Spatial join: which district contains each tree?
# Using st_intersects because trees (points) are first argument
trees_with_districts <- st_join(TREES, DISTRICTS, join = st_intersects)

cat("✓ Spatial join complete!\n")
cat(sprintf("  Joined %s trees to %d districts\n\n", 
            format(nrow(trees_with_districts), big.mark = ","), 
            length(unique(trees_with_districts$CounDist))))

# ============================================================================
# Question 1: Which council district has the most trees?
# ============================================================================

cat("Question 1: Which council district has the most trees?\n")

q1_answer <- trees_with_districts %>%
  st_drop_geometry() %>%
  filter(!is.na(CounDist)) %>%
  count(CounDist, sort = TRUE) %>%
  slice(1)

cat(sprintf("Answer: District %d has the most trees with %s trees\n\n", 
            q1_answer$CounDist, 
            format(q1_answer$n, big.mark = ",")))

# ============================================================================
# Question 2: Which district has the highest density of trees?
# ============================================================================

cat("Question 2: Which council district has the highest density of trees?\n")

q2_answer <- trees_with_districts %>%
  st_drop_geometry() %>%
  filter(!is.na(CounDist)) %>%
  count(CounDist) %>%
  left_join(
    DISTRICTS %>% st_drop_geometry() %>% select(CounDist, Shape__Area),
    by = "CounDist"
  ) %>%
  mutate(tree_density = n / Shape__Area * 1e6) %>%  # Trees per million sq units
  arrange(desc(tree_density)) %>%
  slice(1)

cat(sprintf("Answer: District %d has the highest density with %.2f trees per million square units\n\n", 
            q2_answer$CounDist, 
            q2_answer$tree_density))

# ============================================================================
# Question 3: Which district has highest fraction of dead trees?
# ============================================================================

cat("Question 3: Which district has the highest fraction of dead trees?\n")

# Check what values exist in the status column
cat("Status values in data:\n")
print(table(trees_with_districts$status, useNA = "ifany"))
cat("\n")

q3_answer <- trees_with_districts %>%
  st_drop_geometry() %>%
  filter(!is.na(CounDist)) %>%
  group_by(CounDist) %>%
  summarise(
    total_trees = n(),
    dead_trees = sum(status == "Dead", na.rm = TRUE),
    fraction_dead = dead_trees / total_trees
  ) %>%
  arrange(desc(fraction_dead)) %>%
  slice(1)

cat(sprintf("Answer: District %d has the highest fraction of dead trees at %.2f%% (%d dead out of %d total)\n\n", 
            q3_answer$CounDist, 
            q3_answer$fraction_dead * 100,
            q3_answer$dead_trees,
            q3_answer$total_trees))

# ============================================================================
# Question 4: What is the most common tree species in Manhattan?
# ============================================================================

cat("Question 4: What is the most common tree species in Manhattan?\n")

# First, add borough column based on district number
trees_with_borough <- trees_with_districts %>%
  st_drop_geometry() %>%
  filter(!is.na(CounDist)) %>%
  mutate(
    borough = case_when(
      CounDist >= 1 & CounDist <= 10 ~ "Manhattan",
      CounDist >= 11 & CounDist <= 18 ~ "Bronx",
      CounDist >= 19 & CounDist <= 32 ~ "Queens",
      CounDist >= 33 & CounDist <= 48 ~ "Brooklyn",
      CounDist >= 49 & CounDist <= 51 ~ "Staten Island",
      TRUE ~ NA_character_
    )
  )

q4_answer <- trees_with_borough %>%
  filter(borough == "Manhattan") %>%
  count(spc_common, sort = TRUE) %>%
  slice(1)

cat(sprintf("Answer: The most common tree species in Manhattan is '%s' with %s trees\n\n", 
            q4_answer$spc_common, 
            format(q4_answer$n, big.mark = ",")))

# ============================================================================
# Question 5: What is the species of the tree closest to Baruch's campus?
# ============================================================================

cat("Question 5: What is the species of the tree closest to Baruch's campus?\n")

# Baruch College coordinates (approximate main campus location)
# Address: 55 Lexington Ave, New York, NY 10010
# Coordinates: 40.7406, -73.9832

# Create helper function for making points
new_st_point <- function(lat, lon, ...){
  st_sfc(st_point(c(lon, lat))) %>%  # Note: lon, lat order for st_point!
    st_set_crs("WGS84")
}

# Create point for Baruch College
baruch_point <- new_st_point(lat = 40.7406, lon = -73.9832)

# Calculate distances and find closest tree
q5_answer <- trees_with_districts %>%
  mutate(distance = st_distance(geometry, baruch_point)) %>%
  arrange(distance) %>%
  slice(1)

cat(sprintf("Answer: The tree closest to Baruch's campus is a '%s'\n", 
            q5_answer$spc_common))
cat(sprintf("        Distance: %.2f meters\n", 
            as.numeric(q5_answer$distance)))
cat(sprintf("        Tree ID: %s\n", q5_answer$tree_id))
cat(sprintf("        Health: %s\n\n", q5_answer$health))

# ============================================================================
# Summary of Results
# ============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  Task 4: District-Level Analysis - Summary                  ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")
cat("\n")
cat(sprintf("1. District with most trees: %d (%s trees)\n", 
            q1_answer$CounDist, format(q1_answer$n, big.mark = ",")))
cat(sprintf("2. District with highest density: %d (%.2f trees per million sq units)\n", 
            q2_answer$CounDist, q2_answer$tree_density))
cat(sprintf("3. District with highest dead tree fraction: %d (%.2f%%)\n", 
            q3_answer$CounDist, q3_answer$fraction_dead * 100))
cat(sprintf("4. Most common Manhattan species: %s\n", q4_answer$spc_common))
cat(sprintf("5. Tree closest to Baruch: %s (%.2f m away)\n", 
            q5_answer$spc_common, as.numeric(q5_answer$distance)))
cat("\n")

# Save the joined dataset for later use
saveRDS(trees_with_districts, "data/mp03/trees_with_districts.rds")
cat("✓ Saved joined dataset to data/mp03/trees_with_districts.rds\n")
