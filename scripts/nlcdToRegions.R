pacman::p_load(terra, dplyr,sf, readr, purrr, googledrive )
#  for the pullAreaFiles function 
source("functions/samplingWorkflowFunctions.R")


# inital download from google drive
drive_auth()
gFiles <- drive_ls(path = "temp",pattern = ".tif")

for(i in 1:nrow(gFiles)){
  print(i)
  name <- gFiles$name[i]
  id <- gFiles$id[i]
  export <-  paste0("data/raw/nlcd/", name)
  if(!file.exists(export)){
    # download 
    drive_download(
      file = id,
      path = export
    )
  }  
}


# extract area counts for each grid of the unique MLRA areas 
files <- pullAreaFiles(featName = "MLRA", size = 10)
index <- 1:nrow(files$spatial)
for(i in index){
  
}

name <- files$name
grids <- getGrids(gridSize = 10)
g1 <- files$spatial[1]

g2 <- prepGrids(grids = grids, subGeo = g1)


t1 <- terra::vect()