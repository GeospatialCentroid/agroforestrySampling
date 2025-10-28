library(targets)
library(tarchetypes) 
library(geotargets)

# Load packages required by your functions
tar_option_set(
  packages = c("sf", "dplyr","purrr","furrr", "targets")
)

# Source the functions (which we will also rename)
source("functions/sampleGridsFunctions.R")

# Define the pipeline
list(
  # establish the AOI 
  tar_target(aoiFile,
             "data/derived/aoi/aoi.gpkg",
             format = "file"),
  # read in the aoi and transform to AEA 
  tar_target(
    name = aoi, 
    command = sf::st_read(aoiFile) |> 
      sf::st_transform(crs = 5070)
  ),
  
  # establish the AOI 
  tar_target(original100kmFile,
             "data/derived/grids/grid100km_aea.gpkg",
             format = "file"),
  # establish the AOI spatial object 
  tar_target(name = original100km,
             command = sf::st_read(original100kmFile)
             ),
  
  # filter 100km to aoi 
  tar_target(
    name = selectAOI100km, 
    command = select100km(original100km, aoi)
  ),
  # generate 50 km grids 
  tar_target(
    name = aoi50k, 
    command = buildSubGrids(grids = selectAOI100km, cell_size = 50000,aoi = aoi)
  ),
  # generate 10 km grids 
  ## > 1s 
  tar_target(
    name = aoi10k, 
    command = buildSubGrids(grids = aoi50k, cell_size = 10000,aoi = aoi)
  ),
  # generate 2 km grids 
  ## 11.3 seconds 
  tar_target(
    name = aoi2k, 
    command = buildSubGrids(grids = aoi10k, cell_size = 2000,aoi = aoi)
  ),
  # generate 1 km grids
  ## 4 minutes
  tar_target(
    name = aoi1k, 
    command = buildSubGrids(grids = aoi2k, cell_size = 1000,aoi = aoi)
  )
)