# rough go at the structure, eventually this will be build as a stand along element that analysis could run 

# uncommit if this is not rendered 
# install.packages("pacman")
pacman::p_load(sf, stringr, dplyr, lwgeom, rstudioapi)

# require parameter to unioning data in wgs84 crs 
sf::sf_use_s2(FALSE)

# Replace this path with the folder containing your files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# check to make sure this matches location of the repo 
print(paste("Working directory set to:", getwd()))
# you can also set this manually - shift right click copy path of folder, replace back slash with foward slash 
# setwd("C:/Users/carverd/Desktop/validating SNIC classifications/processedData/processedData/grid_1482-4-4-3-2_2020_bundle/grid_1482-4-4-3-2_2020_bundle") 

# --- HELPER FUNCTION: Read, Merge, Dissolve, Calculate ---
# This function reads multiple files, merges them into one layer, 
# dissolves overlaps, and calculates the total footprint area.
calc_dissolved_area <- function(file_pattern) {
  
  # 1. Find files
  files <- list.files(pattern = file_pattern, full.names = TRUE)
  
  if(length(files) == 0) {
    warning(paste("No files found for:", file_pattern))
    return(0)
  }
  
  # 2. Read all files into a list of SF objects
  sf_list <- lapply(files, function(x) st_read(x, quiet = TRUE))
  
  # 3. Bind them into one single SF dataframe
  combined_sf <- bind_rows(sf_list)
  
  # 4. union into a single geometry to prevent calculating overlapping geographies
  dissolved_sf <- st_union(combined_sf)
  
  # 5. Calculate Area
  total_area <- st_area(dissolved_sf)
  
  # Return numeric value (strip units for easier math)
  return(as.numeric(total_area))
}

# --- 2. PERFORM CALCULATIONS ---

print("Processing AOI...")
# A. AOI (Usually one file, but good to be safe)
aoi_area <- calc_dissolved_area("^aoi_.*\\.gpkg$")

print("Processing TreesFinal...")
# B. TreesFinal
trees_final_area <- calc_dissolved_area("^TreesFinal\\.gpkg$")

print("Processing Trees5 Group (Unioning overlaps)...")
# C. Trees5 Group (Merges Trees5.gpkg and Trees5-2.gpkg, removing overlap)
trees5_area <- calc_dissolved_area("Trees5.*\\.gpkg$")

print("Processing Trees 10/20/40 Group (Unioning overlaps)...")
# D. Trees 10, 20, 40 Group
trees_mixed_area <- calc_dissolved_area("Trees(10|20|40)\\.gpkg$")

print("Processing All Trees Combined...")
# E. ALL TREES (New Addition)
# Catches everything starting with "Trees" (Final, 5, 10, 20, 40, etc.)
# Unions them all to find the total unique vegetation footprint.
all_trees_area <- calc_dissolved_area("^Trees.*\\.gpkg$")


# --- 3. RESULTS ---

if(aoi_area > 0) {
  results <- data.frame(
    Category = c(
      "TreesFinal", 
      "Trees5 Group", 
      "Trees 10/20/40 Group",
      "All Trees Combined"
    ),
    Total_Area_m2 = c(
      trees_final_area, 
      trees5_area, 
      trees_mixed_area,
      all_trees_area
    ),
    Percent_of_AOI = c(
      (trees_final_area / aoi_area) * 100,
      (trees5_area / aoi_area) * 100,
      (trees_mixed_area / aoi_area) * 100,
      (all_trees_area / aoi_area) * 100
    )
  )
  
  # Formatting for readability
  results$Total_Area_m2 <- round(results$Total_Area_m2, 2)
  results$Percent_of_AOI <- round(results$Percent_of_AOI, 2)
  
  print("------------------------------------------------")
  print(paste("Total AOI Area:", round(aoi_area, 2)))
  print("------------------------------------------------")
  print(results)
  
  # export 
  write.csv(x = results, file = "areaSummaryResults.csv", row.names = FALSE)
  
} else {
  print("Error: AOI Area is 0. Please check your AOI file.")
}

