library(targets)
library(tarchetypes)
library(geotargets)

# Load packages required by your functions
tar_option_set(
  packages = c("sf", "dplyr", "purrr", "furrr", "targets")
)

# Source the functions (which we will also rename)
source("functions/sampleGridsFunctions.R")

# Define the pipeline
list(
  tar_file_read(
    name = config,
    # This path is relative to the project root
    command = "_target_config.yml",
    read = yaml::read_yaml(!!.x)
  ),
  # establish the AOI
  tar_target(
    aoi_attPath,
    command = "data/derived/aoi/aoi.gpkg",
    format = "file"
  ),
  tar_target(
    name = aoi_att,
    command = sf::st_read(aoi_attPath)
  ),
  # establish the MLRA grids 
  tar_target(
    mlraGrid_attPath,
    paste0(
      "data/derived/grids/",
      config$aoiName,
      "_",
      config$gridSize,
      "_mlra.gpkg"),
    format = "file"
  ),
  ### 
  tar_target(
    name = mlraGrid_att,
    command = sf::st_read(mlraGrid_attPath)
  )
  
  
  ### attribute information to each grid
  ### TOF 10,16,20
  ### - get the reference intersections for each 12 mile grid  
  ### - use that to pull in possible imagery
  
  ### riparian area
  ### NLCD coverage 10,16,20
  ### road length
  ###
)
