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
overwrite = FALSE
if(!file.exists(export) | isTRUE(overwrite)){
  sf::st_write(aoi,export, delete_dsn  = TRUE)
}
source("0_preprocessing.R")
# produce cropped sub grid areas for all mlra and lrr areas at 10km, 2km, and 1km 
source("1_developGridsForAOI.R")

# attribute data to all grids 
## TOF measure per year  
## NLCD cover per year 
## road network 



