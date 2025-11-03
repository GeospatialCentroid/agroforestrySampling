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
    gridFile,
    paste0(
      "data/derived/grids/",
      config$aoiName,
      "_",
      config$gridSize,
      ".gpkg",
      format = "file"
    ),
    # read in the aoi and transform to AEA
  ),
  ### attribute information to each grid
  ### TOF 10,16,20
  ### riparian area
  ### NLCD coverage 10,16,20
  ### road length
  ###
)
