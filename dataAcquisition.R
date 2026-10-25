# ============================================================================
# Mini Project 03 - Data Acquisition Script
# STA9750: Software Tools for Data Analysis
# ============================================================================

# Load required packages
get_council_districts <- function() {
  cat("\n=== Downloading NYC Council District Boundaries ===\n")
  
  # Use ArcGIS Hub - NYC Planning's data portal
  api_url <- "https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_City_Council_Districts/FeatureServer/0/query"
  geojson_file <- "data/mp03/council_districts_arcgis.geojson"
  
  if(!file.exists(geojson_file)) {
    cat("Downloading from ArcGIS Hub...\n")
    
    resp <- request(api_url) %>%
      req_url_query(
        where = "1=1",           # Get all records
        outFields = "*",          # All fields
        returnGeometry = "true",  # Include geometries
        f = "geojson"            # GeoJSON format
      ) %>%
      req_headers(`User-Agent` = "Mozilla/5.0 (Educational Project)") %>%
      req_perform()
    
    writeBin(resp_body_raw(resp), geojson_file)
    cat("✓ Downloaded GeoJSON file\n")
  } else {
    cat("✓ GeoJSON file already exists\n")
  }
  
  cat("Reading GeoJSON...\n")
  districts <- st_read(geojson_file, quiet = TRUE)
  
  if(all(st_is_empty(districts))) {
    stop("ERROR: Downloaded data has empty geometries!")
  }
  
  cat("Transforming to WGS84...\n")
  districts <- st_transform(districts, crs = "WGS84")
  
  cat("✓ Successfully loaded", nrow(districts), "council districts\n")
  
  return(districts)
}

# Delete the old file first
if(file.exists("data/mp03/council_districts.geojson")) {
  file.remove("data/mp03/council_districts.geojson")
}

# Run it!
DISTRICTS <- get_council_districts()

# Verify
cat("\n=== Verification ===\n")
cat("Number of districts:", nrow(DISTRICTS), "\n")
cat("Has valid geometries:", !all(st_is_empty(DISTRICTS)), "\n")

# Quick plot
plot(st_geometry(DISTRICTS), main = "NYC Council Districts", 
     col = "lightblue", border = "darkblue")

cat("\n✅ SUCCESS!\n")
# ============================================================================
# TASK 2: Download NYC Tree Points from API
# ============================================================================

# ============================================================================
# Download NYC Tree Points
# ============================================================================

get_tree_points <- function(limit = 50000) {
  cat("\n=== Downloading NYC Tree Points ===\n")
  
  # Base URL for the Tree Points API
  base_url <- "https://data.cityofnewyork.us/resource/uvpi-gqnh.geojson"
  
  # Initialize
  offset <- 0
  all_files <- list()
  page_num <- 1
  
  # Keep downloading until we get less than 'limit' results
  repeat {
    filename <- file.path("data/mp03", sprintf("trees_page_%03d.geojson", page_num))
    
    # Check if file already exists
    if(file.exists(filename)) {
      cat(sprintf("✓ Page %d already downloaded\n", page_num))
      all_files[[page_num]] <- filename
      
      # Check if we got fewer results (end of dataset)
      test_data <- st_read(filename, quiet = TRUE)
      if(nrow(test_data) < limit) {
        cat("✓ Reached end of dataset\n")
        break
      }
      
      offset <- offset + limit
      page_num <- page_num + 1
      next
    }
    
    # Build request
    cat(sprintf("Downloading page %d (offset: %d)...\n", page_num, offset))
    
    resp <- request(base_url) %>%
      req_url_query(`$limit` = limit, `$offset` = offset) %>%
      req_headers(`User-Agent` = "Mozilla/5.0 (Educational Project)") %>%
      req_retry(max_tries = 3) %>%
      req_perform()
    
    # Save response to file
    writeBin(resp_body_raw(resp), filename)
    all_files[[page_num]] <- filename
    cat(sprintf("✓ Saved page %d\n", page_num))
    
    # Read to check how many records we got
    current_data <- st_read(filename, quiet = TRUE)
    n_records <- nrow(current_data)
    cat(sprintf("  Retrieved %d records\n", n_records))
    
    # If we got fewer than limit, we've reached the end
    if(n_records < limit) {
      cat("✓ Reached end of dataset\n")
      break
    }
    
    # Update for next iteration
    offset <- offset + limit
    page_num <- page_num + 1
    
    # Be polite - add a small delay
    Sys.sleep(0.5)
  }
  
  # Read and combine all files
  cat("\nCombining all tree point files...\n")
  tree_list <- lapply(all_files, function(f) {
    st_read(f, quiet = TRUE)
  })
  
  trees <- bind_rows(tree_list)
  cat("✓ Successfully loaded", format(nrow(trees), big.mark = ","), "trees\n")
  
  return(trees)
}

# Run it!
cat("\n🌳 Starting tree download (this will take 5-15 minutes)...\n")
TREES <- get_tree_points()

cat("\n✅ ALL DATA DOWNLOADED!\n")
cat("You now have:\n")
cat("  • DISTRICTS:", nrow(DISTRICTS), "council districts\n")
cat("  • TREES:", format(nrow(TREES), big.mark = ","), "trees\n")




# Check the data structures
cat("\n=== Data Summary ===\n")
cat("\nDISTRICTS:\n")
print(names(DISTRICTS))
print(head(DISTRICTS %>% st_drop_geometry(), 3))

cat("\nTREES:\n")
print(names(TREES))
print(head(TREES %>% st_drop_geometry() %>% select(1:5), 3))

# Quick preview plot
cat("\nCreating preview map...\n")
plot(st_geometry(DISTRICTS), main = "NYC: Districts & Trees", 
     border = "black", lwd = 2)
plot(st_geometry(TREES), add = TRUE, pch = ".", col = alpha("darkgreen", 0.3))




# Save both datasets so you don't have to re-download
saveRDS(DISTRICTS, "data/mp03/districts.rds")
saveRDS(TREES, "data/mp03/trees.rds")

cat("✅ Data saved! You can reload anytime with:\n")
cat("   DISTRICTS <- readRDS('data/mp03/districts.rds')\n")
cat("   TREES <- readRDS('data/mp03/trees.rds')\n")


