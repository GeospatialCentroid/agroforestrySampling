pacman::p_load(
  targets,
  tarchetypes,
  geotargets,
  future,
  future.callr,
  stringr,
  tidyr,
  dplyr,
  terra
)
# seems to be automatically applied and dynamic branching targets
plan(future.callr::callr, workers = 4) # Adjust number of workers as needed

# Load packages required by your functions
tar_option_set(
  packages = c(
    "sf",
    "dplyr",
    "purrr",
    "furrr",
    "targets",
    "stringr",
    "tidyr",
    "tibble",
    "tidyr"
  ),
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
  #
  tar_target(
    name = mlraGrid_att,
    command = sf::st_read(mlraGrid_attPath)
  ),

  # TOF grid processing ----------------------------------------------------
  # 12 mile reference grid path
  tar_target(
    name = grid12_Path,
    command = "data/raw/grid12M/twelve_mi_grid_uid.gpkg",
    format = "file"
  ),
  # 12 mile reference grid
  tar_target(
    name = grid12,
    command = sf::st_read(grid12_Path)
  ),
  # cot over time file paths
  tar_target(
    name = cotPaths,
    command = gatherTOF_Files()
  ),
  # riparian raster
  tar_terra_rast(
    name = riparian,
    command = gatherRiparian()
  ),
  ## primary and secondary roads... not the most detail but easiest pull at the state level
  tar_target(
    name = roads,
    command = gatherRoads(aoi = aoi_att, stateFIPS = config$states)
  ),
  tar_terra_rast(
    name = nlcd_2010,
    command = gatherNLCD(
      aoi = aoi_att,
      nlcd_path = "data/raw/nlcd/Annual_NLCD_LndCov_2010_CU_C1V1.tif"
    )
  ),
  tar_terra_rast(
    name = nlcd_2016,
    command = gatherNLCD(
      aoi = aoi_att,
      nlcd_path = "data/raw/nlcd/Annual_NLCD_LndCov_2016_CU_C1V1.tif"
    )
  ),
  tar_terra_rast(
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
  # vect of the mlra data
  tar_terra_vect(
    name = mlraAtt,
    command = terra::vect(mlraPath_att)
  ),
  # Calculate riparian area by MLRA (all 15 features)
  tar_target(
    name = riparian_by_mlra,
    command = calculateRastByMLRA(
      rast = riparian,
      mlra_vect = mlraAtt
    )
  ),
  # Calculate nlcd area by MLRA (all 15 features)
  tar_target(
    name = nlcd10_by_mlra,
    command = calculateRastByMLRA(
      rast = nlcd_2010,
      mlra_vect = mlraAtt
    )
  ),
  # Calculate nlcd area by MLRA (all 15 features)
  tar_target(
    name = nlcd16_by_mlra,
    command = calculateRastByMLRA(
      rast = nlcd_2016,
      mlra_vect = mlraAtt
    )
  ),
  # Calculate nlcd area by MLRA (all 15 features)
  tar_target(
    name = nlcd20_by_mlra,
    command = calculateRastByMLRA(
      rast = nlcd_2020,
      mlra_vect = mlraAtt
    )
  ),
  # calculate roads in each MLRA
  tar_target(
    name = roads_by_mlra,
    command = calculateRoadsByMLRA(roads_vect = roads, mlra_vect = mlraAtt)
  ),
  # generate a list of tof per MLRA area
  tar_target(
    name = tof_by_mlra,
    command = calculateTotalTOF(
      cot_paths = cotPaths,
      grid_12 = grid12,
      mlra_vect = mlraAtt
    )
  ),

  # summarize data to the 1km grids  ---------------------------------------
  # format the grid feature
  tar_target(
    mlra_id_list,
    unique(mlraGrid_att$MLRA_ID)
  ),
  # map over riparian
  tar_target(
    ripArea_sugGrids,
    processSubGridAreas(
      raster_layer = riparian,
      grid_feature = mlraGrid_att,
      current_mlra_id = mlra_id_list
    ),
    pattern = map(mlra_id_list)
  )
  # # map over branches for NlCD2010
  # tar_target(
  #   nlcd_10_sugGrids,
  #   processSubGridAreas(raster_layer = nlcd_2010, grid_feature = mlra_groups),
  #   pattern = map(mlra_groups)
  # ),
  # # map over nlcd 2016
  # tar_target(
  #   nlcd_16_sugGrids,
  #   processSubGridAreas(raster_layer = nlcd_2016, grid_feature = mlra_groups),
  #   pattern = map(mlra_groups)
  # ),
  # # map over nlcd 2020
  # tar_target(
  #   nlcd_20_sugGrids,
  #   processSubGridAreas(raster_layer = nlcd_2020, grid_feature = mlra_groups),
  #   pattern = map(mlra_groups)
  # ),
)
