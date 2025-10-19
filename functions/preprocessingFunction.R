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
  lower48 <- rawStatesData %>%
    dplyr::filter(!name %in% exclude) |>
    dplyr::select(name, postal)
  
  return(lower48)
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