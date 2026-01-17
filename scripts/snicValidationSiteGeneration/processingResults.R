# Load libraries
library(terra)
library(dplyr)

# --- FUNCTION 1: Get Total Raster Area ---
get_tif_area <- function(search_pattern = "\\.tif$") {
  
  files <- list.files(pattern = search_pattern, full.names = TRUE, ignore.case = TRUE)
  
  if (length(files) == 0) {
    message("No TIF files found.")
    return(NULL)
  }
  
  # Load the raster
  r <- rast(files[1])[[1]]
  
  # Calculate Expanse
  # Note: expanse() returns a numeric vector by default for single layers.
  val <- expanse(r, unit = "m", transform = TRUE)
  
  # Create DataFrame
  total_area <- data.frame(
    feature = "Total Raster Extent",
    area = round(as.numeric(val$area), 2)
  )
  
  return(total_area)
}

# --- FUNCTION 2: Union, Dissolve, and Sum 'M' or 'T' ---
union_and_dissolve <- function(prefixes) {
  
  # 1. Create Pattern
  pattern_string <- paste0("^(", paste(prefixes, collapse="|"), ").*\\.gpkg$")
  
  # 2. Find files
  target_files <- list.files(pattern = pattern_string, ignore.case = TRUE, full.names = TRUE)
  
  if(length(target_files) == 0) {
    # Return a 0 area dataframe if no files found, so the script doesn't crash later
    empty_df <- data.frame(feature = paste(prefixes, collapse=", "), area = 0)
    return(list(summary = empty_df, spatial = NULL))
  }
  
  message(paste("Processing", length(target_files), "files for prefix:", paste(prefixes, collapse=", ")))
  
  # 3. Read and Combine
  vector_list <- lapply(target_files, vect)
  all_vectors <- vect(vector_list)
  
  # 4. Dissolve (Aggregate)
  dissolved_feature <- aggregate(all_vectors, dissolve = TRUE)
  
  # 5. Calculate Area
  total_area <- sum(expanse(dissolved_feature, unit = "m"))
  
  # 6. Create Summary DataFrame
  summary_df <- data.frame(
    feature = paste(prefixes, collapse = ", "),
    area = round(total_area, 2)
  )
  
  return(list(
    summary = summary_df,
    spatial = dissolved_feature
  ))
}

# =======================================================
# EXECUTION & COMBINATION WORKFLOW
# =======================================================

# 1. Run the calculations
tif_result <- get_tif_area()
m_result   <- union_and_dissolve("m")
t_result   <- union_and_dissolve("t")

# Check if TIF data exists before proceeding
# Check if TIF data exists before proceeding
if (!is.null(tif_result)) {
  
  # 2. Extract the specific summary rows
  df_total <- tif_result
  df_m     <- m_result$summary
  df_t     <- t_result$summary
  
  # --- RENAME FEATURES ---
  # Overwrite the default "m" and "t" labels with descriptive names
  df_m$feature <- "No trees"
  df_t$feature <- "Trees"
  
  # 3. Calculate "Area Not Within M and T"
  # Logic: Total Tif Area - (Area M + Area T)
  occupied_area <- df_m$area + df_t$area
  remnant_area  <- df_total$area - occupied_area
  
  df_remnant <- data.frame(
    feature = "Unclassified (Not M or T)",
    area = round(remnant_area, 2)
  )
  
  # 4. Combine into one Master Table
  # We stack Trees, No trees, and Unclassified first
  final_df <- rbind(df_m, df_t, df_remnant)
  
  # 5. Calculate Percentages
  # We use the Total Tif Area as the denominator
  final_df$percent_of_total <- round((final_df$area / df_total$area) * 100, 2)
  
  # 6. Add the Grand Total row at the bottom for reference
  df_total$percent_of_total <- 100
  final_df <- rbind(final_df, df_total)
  
  # 7. Print Final Report
  print("--- Final Area Summary ---")
  print(final_df)
  
  # Optional: Save to CSV
  write.csv(final_df, "area_summary_report.csv", row.names = FALSE)
  
} else {
  message("Could not generate report: TIF file missing.")
}

# Install these if you don't have them
# install.packages(c("leaflet", "sf", "leafem"))

library(leaflet)
library(sf)
library(leafem) # Essential for plotting RGB rasters in Leaflet
# install.packages("leafem")
# --- PREPARE DATA ---

# 1. Convert "Trees" (T) to sf and Transform to Lat/Lon (WGS84)
if (!is.null(t_result$spatial)) {
  # Convert from terra vector to sf object
  t_sf <- st_as_sf(t_result$spatial)
  # Transform to EPSG:4326 (Latitude/Longitude) for Leaflet
  t_sf <- st_transform(t_sf, crs = 4326)
}

# 2. Convert "No Trees" (M) to sf and Transform
if (!is.null(m_result$spatial)) {
  m_sf <- st_as_sf(m_result$spatial)
  m_sf <- st_transform(m_sf, crs = 4326)
}

# 3. Load Raster
naip_file <- list.files(pattern = "^naip.*\\.tif$", full.names = TRUE, ignore.case = TRUE)[1]
if (!is.na(naip_file)) {
  r_naip <- rast(naip_file)
}

# --- GENERATE LEAFLET MAP ---
library(leaflet)
map <- leaflet() %>%
  # Add a standard basemap (optional, provides context if your image has gaps)
  addProviderTiles(providers$CartoDB.Positron, group = "Basemap") %>%
  
  # 1. Add the NAIP RGB Raster
  # addRasterRGB is from the 'leafem' package. 
  # It renders bands 1, 2, 3 as Red, Green, Blue.
  addRasterRGB(r_naip, 
               r = 1, g = 2, b = 3, 
               group = "NAIP Imagery", 
               project = TRUE) %>% # Auto-projects to Web Mercator for display
  
  # 2. Add "Trees" Layer
  addPolygons(data = t_sf,
              color = "darkgreen",      # Border color
              weight = 1, 
              fillColor = "forestgreen", # Fill color
              fillOpacity = 0.5, 
              group = "Trees") %>%       # GROUP NAME is crucial for the toggle
  
  # 3. Add "No Trees" Layer
  addPolygons(data = m_sf,
              color = "brown", 
              weight = 1, 
              fillColor = "tan", 
              fillOpacity = 0.5, 
              group = "No Trees") %>%
  
  # 4. Add the Toggle Control (Layer Control)
  addLayersControl(
    baseGroups = c("Basemap"),
    overlayGroups = c("NAIP Imagery", "Trees", "No Trees"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Display the map
map


