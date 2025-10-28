# primary workflow script for the sampling processing 

# generate the required folder structure 
source("scripts/createFolderStructure.R")

# generate the non AOI dependent data 
source("0_preprocessing.R")
## could add structure for pulling the NLCD or MLRA data, even if just from google drive

# generate aoi species grids 
## this could be a vector of state names, MLRA_ID, or LRRSYM  
## type must be with "state","lrr","mlra"
source("scripts/generateAOI.R")
aoi <- generateAOI(id = "Nebraska", type = "state")
# need to write this so it can becomes a target for the next workflow 
export <- "data/derived/aoi/aoi.gpkg"
overwrite = TRUE
if(!file.exists(export) | isTRUE(overwrite)){
  sf::st_write(aoi,export, delete_dsn  = TRUE)
}
source("1_developGridsForAOI.R")

