# _targets.R

library(targets)
library(tarchetypes) 

# Load packages required by your functions
tar_option_set(
  packages = c("sf", "dplyr", "rnaturalearth", "tigris")
)

# Source the functions (which we will also rename)
source("functions/preprocessingFunction.R")

# Define the pipeline
list(

  # State and County Processing ---------------------------------------------
  # Target 1: Download the raw data as an R object
  tar_target(
    name = rawStatesData,
    command = downloadRawStates()
  ),
  
  # Target 2: Process the raw data to get an R object for the lower 48
  # This target depends on the `rawStatesData` target
  tar_target(
    name = lower48Data,
    command = processLower48(rawStatesData)
  ),
  
  # Target 3: Save the raw data object to a file
  # This depends on `rawStatesData`
  tar_target(
    name = rawStatesFile,
    command = saveGeopackageSF(rawStatesData, "data/raw/us/allStates.gpkg"),
    format = "file"
  ),?
  
  # Target 4: Save the processed data object to a file
  # This depends on `lower48Data`
  tar_target(
    name = lower48File,
    command = saveGeopackageSF(lower48Data, "data/derived/us/lower48.gpkg"),
    format = "file"
  ),
  # 5. Download all US counties from tigris
  tar_target(
    name = tigrisCountiesData, # Renamed for clarity
    command = downloadTigrisCounties() # Changed function call
  ),
  
  # 6. Filter counties to only those in the lower 48
  tar_target(
    name = lower48CountiesData,
    command = filterCountiesToLower48(tigrisCountiesData, lower48Data) # Uses new tigris data
  ),
  
  # 7. Save the filtered counties to a file
  tar_target(
    name = lower48CountiesFile,
    command = saveGeopackageSF(lower48CountiesData, "data/derived/us/lower48_counties.gpkg"),
    format = "file"
  ),
  
  #8. bounding box of the lower 48 
  tar_target(
    name = extent48,
    command = lower48Extent(lower48Data),
    
  ),
  #9. export the extent 
  tar_target(
    name = extent48File,
    command = saveGeopackageSF(extent48, "data/derived/us/extent48.gpkg"),
    format = "file"
  )

  # MLRA processing ---------------------------------------------------------
  # process MLRA data 
  # tar_target(
  #   name = mlraPath, 
  #   command = {"data/raw/mlra/MLRA_52_2022/MLRA_52.shp"},
  #   format = "file"
  # )
  
  # tar_target(
  #   name = lower48MLRA,
  #   command = processMLRA(mlraPath, extent48)
  # ),
  # 
  # # 7. Save the filtered counties to a file
  # tar_target(
  #   name = lower48MLRAFile,
  #   command = saveGeopackageTerra(lower48MLRA, "data/derived/mlra/lower48MLRA.gpkg"),
  #   format = "file"
  # )
  
  ## format to CRS and lower 48 
  ## spilt into MLRA and LRR 
  ## export results 
  
  
)


