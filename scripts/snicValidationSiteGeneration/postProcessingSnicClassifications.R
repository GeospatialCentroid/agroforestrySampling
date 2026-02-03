# rough go at the structure, eventually this will be build as a stand along element that analysis could run 


pacman::p_load(sf, stringr, dplyr)

source("functions/naipScrape.R") # only works for 2016 and 2020 

## temp stuff 
# sites of immediate concern 
sites <- c("1415-3-12-4-1", "1413-1-6-15-2", "1482-4-4-3-2","1544-1-18-1-4", "1352-3-5-c-1")
grid100 <- sf::st_read("data/derived/grids/grid100km_aea.gpkg")

# Generate the 1km grid object for reference 
for(i in sites){
  exportPath <- paste0("data/derived/snicExports/aoi_",i,".gpkg")
  area <- getAOI(grid100 = grid100, id = i)
  sf::st_write(obj = area, dsn = exportPath)  
}


# Replace this path with the folder containing your files
setwd("C:/path/to/your/data") 

# This function finds files matching a pattern, sums their area, and returns the value
calculate_category_area <- function(file_pattern) {
  # List files that match the pattern (case insensitive)
  files <- list.files(pattern = file_pattern, full.names = TRUE)
  
  if(length(files) == 0) {
    warning(paste("No files found for pattern:", file_pattern))
    return(0)
  }
  
  # Loop through files, read them, and sum their areas
  total_area <- 0
  for (f in files) {
    # Read the layer (silently to keep console clean)
    layer <- st_read(f, quiet = TRUE)
    
    # Calculate area. st_area returns value with units (e.g., m^2). 
    # We convert to numeric to simplify math.
    area_val <- sum(st_area(layer))
    total_area <- total_area + as.numeric(area_val)
  }
  
  return(total_area)
}

# --- 2. CALCULATE AREAS ---

# A. AOI Area
# Matches files starting with "aoi" (e.g., aoi_1482-4-4-3-2.gpkg)
aoi_area <- calculate_category_area("^aoi_.*\\.gpkg$")

# B. TreesFinal Area
# Matches exactly "TreesFinal.gpkg"
trees_final_area <- calculate_category_area("^TreesFinal\\.gpkg$")

# C. Trees5 Area (Sum of all Tree5 files)
# Matches "Trees5" followed by anything (covers Trees5.gpkg and Trees5-2.gpkg)
trees5_area <- calculate_category_area("Trees5.*\\.gpkg$")

# D. Trees 10, 20, 40 Area (Sum of these specific files)
# Matches Trees10, Trees20, OR Trees40
trees_mixed_area <- calculate_category_area("Trees(10|20|40)\\.gpkg$")


# --- 3. CALCULATE PERCENTAGES & PRINT RESULTS ---

# Prevent division by zero if AOI is missing
if(aoi_area > 0) {
  
  # Create a summary dataframe for clean output
  results <- data.frame(
    Category = c("TreesFinal", "Trees5 Group", "Trees 10/20/40 Group"),
    Total_Area_m2 = c(trees_final_area, trees5_area, trees_mixed_area),
    Percent_of_AOI = c(
      (trees_final_area / aoi_area) * 100,
      (trees5_area / aoi_area) * 100,
      (trees_mixed_area / aoi_area) * 100
    )
  )
  
  print(paste("Total AOI Area:", round(aoi_area, 2)))
  print("------------------------------------------------")
  print(results)
  
} else {
  print("Error: AOI Area is 0 or AOI file not found. Check filename pattern.")
}