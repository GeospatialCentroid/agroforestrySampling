library(targets)
library(tarchetypes)
library(geotargets)

# Load packages required by your functions
tar_option_set(
  packages = c("sf", "dplyr", "purrr", "furrr", "targets", "yaml")
)

# Source the functions (which we will also rename)
source("functions/sampleGridsFunctions.R")
# temporary environment
temp_env <- new.env()
source("functions/preprocessingFunction.R", local = temp_env)
#  Copy  functions you want into your main environment
saveGeopackageSF <- temp_env$saveGeopackageSF
# Remove the temporary environment
rm(temp_env)

# Define the pipeline
list(
  tar_file_read(
    name = config,
    # This path is relative to the project root
    command = "_target_config.yml",
    read = yaml::read_yaml(!!.x)
  ),

  # establish the AOI
  tar_target(aoiFile, "data/derived/aoi/aoi.gpkg", format = "file"),
  # read in the aoi and transform to AEA
  tar_target(
    name = aoi,
    command = sf::st_read(aoiFile) |>
      sf::st_transform(crs = 5070)
  ),

  # establish the AOI
  tar_target(
    original100kmFile,
    "data/derived/grids/grid100km_aea.gpkg",
    format = "file"
  ),
  # establish the AOI spatial object
  tar_target(name = original100km, command = sf::st_read(original100kmFile)),

  # filter 100km to aoi
  tar_target(
    name = selectAOI100km,
    command = select100km(original100km, aoi)
  ),
  # generate 50 km grids
  tar_target(
    name = aoi50k,
    command = buildSubGrids(
      grids = selectAOI100km,
      cell_size = 50000,
      aoi = aoi
    )
  ),
  # generate 10 km grids
  ## > 1s
  tar_target(
    name = aoi10k,
    command = buildSubGrids(grids = aoi50k, cell_size = 10000, aoi = aoi)
  ),
  # generate 2 km grids
  ## 11.3 seconds
  tar_target(
    name = aoi2k,
    command = buildSubGrids(grids = aoi10k, cell_size = 2000, aoi = aoi)
  ),
  # generate 1 km grids
  ## 4 minutes
  tar_target(
    name = aoi1k,
    command = buildSubGrids(grids = aoi2k, cell_size = 1000, aoi = aoi)
  ),
  # export files for the next targets workflow
  ## waiting on this for now and will just u
  tar_target(
    name = export1k,
    command = saveGeopackageSF(
      sfObject = aoi1k,
      outputPath = paste0("data/derived/grids/", config$aoiName, "_1km.gpkg")
    )
  ), 
  
  # process the MLRA grids  -------------------------------------------------
  ## read in the AOI record but keep it in WGS84 
  tar_target(
    name = aoi84,
    command = sf::st_read(aoiFile)
  ),
  tar_target(
    name = grids84,
    command = sf::st_transform(aoi1k, crs = 4326)
  ),
  # load in the MLRA data 
  # establish the AOI
  tar_target(mlraFile, "data/derived/mlra/lower48MLRA.gpkg", format = "file"),
  # read in the aoi and transform to AEA
  tar_target(
    name = mlraSample,
    command = sf::st_read(mlraFile)
  ),
  tar_target(
    name = mlraCrop,
    command = cropToAOI(
      object = mlraSample,
      aoi = aoi84
    )
  ),
  tar_target(
    name = exportMLRACrop,
    command = saveGeopackageSF(
      sfObject = mlraCrop,
      outputPath = paste0("data/derived/mlra/", config$aoiName, "_MLRA.gpkg")
    )
  ), 

  
  # prep for the group function 
  tar_target(mlraGroups, unique(mlraCrop$MLRA_ID)),
  # apply the itorative elements 
  tar_target(
    mlra_1kGrids,
    command = cropGridsMLRA(mlra_Group = mlraGroups,
                        mlra = mlraCrop,
                        grids_84 = grids84),
    pattern = map(mlraGroups)
  ),
  tar_target(
    name = mlra_1kGridsExport,
    command = saveGeopackageSF(
      sfObject = mlra_1kGrids,
      outputPath = paste0("data/derived/grids/", config$aoiName, "_",config$gridSize,"_mlra.gpkg")
    )
  ),
  # establish the AOI
  tar_target(lrrFile, "data/derived/mlra/lower48LRR.gpkg", format = "file"),
  # read in the aoi and transform to AEA
  tar_target(
    name = lrrSample,
    command = sf::st_read(lrrFile)
  ),
  tar_target(
    name = lrrCrop,
    command = cropToAOI(
      object = lrrSample,
      aoi = aoi84
    )
  ),
  tar_target(
    name = exportLLRCrop,
    command = saveGeopackageSF(
      sfObject = lrrCrop,
      outputPath = paste0("data/derived/mlra/", config$aoiName, "_LRR.gpkg")
    )
  ), 
  # prep for the group function 
  tar_target(lrrGroups, unique(mlraCrop$LRRSYM)),
  # apply the itorative elements to the LRR 
  tar_target(
    lrr_1kGrids,
    command = cropGridsLRR(lrr_Group = lrrGroups,
                        lrr = lrrCrop,
                        grids_84 = grids84),
    pattern = map(lrrGroups)
  ),
  tar_target(
    name = lrr_1kGridsExport,
    command = saveGeopackageSF(
      sfObject = lrr_1kGrids,
      outputPath = paste0("data/derived/grids/", config$aoiName, "_",config$gridSize,"_lrr.gpkg")
    )
  ) 
)
