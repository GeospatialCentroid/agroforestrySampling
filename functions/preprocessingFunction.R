# R/functions.R

# Function to download the raw data
downloadRawStates <- function() {
  # The ne_states() function downloads the data
  rnaturalearth::ne_states(country = "United States of America",
                           returnclass = "sf")
}

# Function to process the raw data into the lower 48
processLower48 <- function(rawStatesData) {
  # Define the states/territories to exclude
  exclude <- c("Alaska", 
               "Hawaii", 
               "Puerto Rico", 
               "U.S. Virgin Islands", 
               "Guam", 
               "Commonwealth of the Northern Mariana Islands",
               "American Samoa")
  
  # Filter the sf dataframe
  lower48 <- rawStatesData |>
    dplyr::filter(!name %in% exclude) |>
    dplyr::select(name, postal)
  
  return(lower48)
}

downloadTigrisCounties <- function() {
  # Download US counties from tigris
  # cb = TRUE gives cartographic boundary files (smaller, generalized)
  # resolution = "500k" is a good balance of detail and size
  tigris::counties(cb = TRUE, resolution = "500k")
}

# Updated to use st_join and handle different data columns
filterCountiesToLower48 <- function(tigrisCountiesData, lower48StatesData) {
  
  # Ensure CRS matches for a valid spatial join
  # rnaturalearth (states) is WGS84 (EPSG:4326)
  # tigris (counties) is NAD83 (EPSG:4269)
  # We'll transform counties to match the states
  counties_wgs84 <- sf::st_transform(tigrisCountiesData, sf::st_crs(lower48StatesData))
  
  # Spatially join counties to states
  # This adds state 'name' and 'postal' to counties within them
  counties_joined <- sf::st_join(counties_wgs84, lower48StatesData, 
                                 join = sf::st_intersects)
  
  # Filter to keep only counties that successfully joined (i.e., are in the lower 48)
  # and select a clean set of columns
  lower48Counties <- counties_joined %>%
    dplyr::filter(!is.na(name)) %>% # 'name' comes from lower48StatesData
    dplyr::select(
      countyName = NAME,        # from tigris
      countyGeoId = GEOID,       # from tigris
      stateName = name,          # from lower48StatesData
      statePostal = postal       # from lower48StatesData
    )
  
  return(lower48Counties)
}

# A helper function to save any sf object as a .gpkg file
saveGeopackage <- function(sfObject, outputPath) {
  # Ensure the directory exists
  dir.create(dirname(outputPath), recursive = TRUE, showWarnings = FALSE)
  
  # Write the file, overwriting if needed (targets controls the "if needed" part)
  sf::st_write(sfObject, dsn = outputPath, delete_layer = TRUE)
  
  # Return the path for {targets} to track as a file
  return(outputPath)
}