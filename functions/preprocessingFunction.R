# R/functions.R

# Function to download the raw data
downloadRawStates <- function() {
  # The ne_states() function downloads the data
  tigris::states() |>
    sf::st_transform(crs = 4326)
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
    dplyr::filter(!NAME %in% exclude) |>
    dplyr::select("stateName" = NAME,
                  "stateGEOID" = GEOID )
   
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
    dplyr::filter(!is.na(NAME)) %>% # 'name' comes from lower48StatesData
    dplyr::select(
      countyName = NAME,        # from tigris
      countyGeoId = GEOID,       # from tigris
      stateName = NAME,          # from lower48StatesData
      stateGEOID = stateGEOID       # from lower48StatesData
    )
  
  return(lower48Counties)
}

lower48Extent <- function(lower48){
  # convert to terra objects
  # vect48 <- terra::vect(lower48)
  # convert to an bounding box 
  extent <- sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(lower48)))
  
  return(extent)
}


# MLRA data  --------------------------------------------------------------
# process MLRA data 
## format to CRS and lower 48 
## spilt into MLRA and LRR 
## export results 
## url pulled from https://www.nrcs.usda.gov/resources/data-and-reports/major-land-resource-area-mlra
# extent48 <- sf::st_read( "data/derived/us/lower48.gpkg")
# mlraPath <- "data/raw/mlra/MLRA_52_2022/MLRA_52.shp"
processMLRA <- function(mlra_Path, extent_48){
  mlra <- sf::st_read(mlra_Path) |> sf::st_crop(extent_48)
  return(mlra)
}

# aggregate the mlra data to the LRR level 
generateLRR <- function(lower48MLRA){
  lrr <- lower48MLRA |>
    dplyr::group_by(LRRSYM) |>
    dplyr::summarise(
      lrrNAME = dplyr::first(LRR_NAME)
    )
  return(lrr)
}



# generate the 100km grid object  --------------------------------------------------
buildAGrid <- function(extent_object, cell_size){
  # transform to equal area 
  ea <- sf::st_transform(extent_object, 5070)
  # generate grid 
  grid <- sf::st_make_grid(
    x = ea, 
    cellsize = cell_size
  )
  if("id" %in% names(ea)){
    ids <- paste0(ea$id[1],"-",as.hexmode(1:length(grid)))
  }else{
    ids = as.hexmode(1:length(grid))
  }
  # generate ID 
  gridID <- sf::st_sf(
    id = ids,
    geomentry = grid)
  # export 
  return(gridID)
}





# export data  ------------------------------------------------------------

# A helper function to save any sf object as a .gpkg file
saveGeopackageSF <- function(sfObject, outputPath) {
  # Ensure the directory exists
  dir.create(dirname(outputPath), recursive = TRUE, showWarnings = FALSE)
  
  # Write the file, overwriting if needed (targets controls the "if needed" part)
  sf::st_write(sfObject, dsn = outputPath, delete_layer = TRUE)
  
  # Return the path for {targets} to track as a file
  return(outputPath)
}

## avoiding terra objects for a little bit as they are a bit more to handle within targets 
# saveGeopackageTerra <- function(terraObject, outputPath) {
#   # Ensure the directory exists
#   # dir.create(dirname(outputPath), recursive = TRUE, showWarnings = FALSE)
#   
#   # Write the file, overwriting if needed (targets controls the "if needed" part)
#   terra::writeVector(terraObject, outputPath, overwrite = TRUE)
#   
#   # Return the path for {targets} to track as a file
#   return(outputPath)
# }