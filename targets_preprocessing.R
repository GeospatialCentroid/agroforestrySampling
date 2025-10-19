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
    command = saveGeopackage(rawStatesData, "data/raw/us/allStates.gpkg"),
    format = "file"
  ),?
  
  # Target 4: Save the processed data object to a file
  # This depends on `lower48Data`
  tar_target(
    name = lower48File,
    command = saveGeopackage(lower48Data, "data/derived/us/lower48.gpkg"),
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
    command = saveGeopackage(lower48CountiesData, "data/derived/us/lower48_counties.gpkg"),
    format = "file"
  )
 
)
