library(targets)
library(tarchetypes)
library(geotargets)
library(future)
library(future.callr)

plan(future.callr::callr, workers = 8) # Adjust number of workers as needed


# Load packages required by your functions
tar_option_set(
  packages = c("sf", "dplyr", "purrr", "furrr", "targets"),
  controller = crew::crew_controller_local(workers = 4)
)

# Source the functions (which we will also rename)
source("functions/attributeGridsFunctions.R")

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
      "_mlra.gpkg"
    ),
    format = "file"
  ),
  ###
  tar_target(
    name = mlraGrid_att,
    command = sf::st_read(mlraGrid_attPath)
  ),

  # TOF grid processing ----------------------------------------------------
  tar_target(
    name = grid12_Path,
    command = "data/raw/grid12M/twelve_mi_grid_uid.gpkg",
    format = "file"
  ),
  tar_target(
    name = grid12,
    command = sf::st_read(grid12_Path)
  ),
  tar_target(
    name = cotPaths,
    command = gatherTOF_Files()
  ),
  tar_terra_rast(
    name = riparian,
    command = gatherRiparian()
  ),
  ## primary and secondary roads... not the most detail but easiest pull at the state level
  tar_target(
    name = roads,
    command = gatherRoads(aoi = aoi_att, stateFIPS = config$states)
  ),
  tar_target(
    name = nlcd_2010,
    command = gatherNLCD(
      aoi = aoi_att,
      nlcd_path = "data/raw/nlcd/Annual_NLCD_LndCov_2010_CU_C1V1.tif"
    )
  ),
  tar_target(
    name = nlcd_2016,
    command = gatherNLCD(
      aoi = aoi_att,
      nlcd_path = "data/raw/nlcd/Annual_NLCD_LndCov_2016_CU_C1V1.tif"
    )
  ),
  tar_target(
    name = nlcd_2020,
    command = gatherNLCD(
      aoi = aoi_att,
      nlcd_path = "data/raw/nlcd/Annual_NLCD_LndCov_2020_CU_C1V1.tif"
    )
  ),
  # summarystats for MLRA areas --------------------------------------------
  # read in the MLRA objects
  tar_target(
    name = mlraPath_att,
    command = paste0("data/derived/mlra/", config$aoiName, "_MLRA.gpkg"),
    format = "file"
  ),
  tar_terra_vect(
    name = mlraAtt,
    command = terra::vect(mlraPath_att)
  ),
  # Calculate riparian area by MLRA (all 15 features)
  tar_target(
    name = riparian_by_mlra,
    command = calculateRiparianByMLRA(
      riparian_rast = riparian,
      mlra_vect = mlraAtt
    )
  )
)
