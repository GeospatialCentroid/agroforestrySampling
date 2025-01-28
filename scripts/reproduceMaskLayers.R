###
# reproject mask layers from phase 1 of the project to ensure integration with the validation methodology 
#
#
###

pacman::p_load(terra,dplyr,sf)

# load in reference file for crs info 
crs <- terra::vect("data/products/modelGrids/modelGrids_2010.gpkg")

# pull in census places data 
censusPaths <- list.files(
  path = "data/products/urbanMask",
  pattern = ".shp$", # ends in .shp
  full.names = TRUE,
  recursive = TRUE
)

# pull in nlcd files 
nlcdPaths <- list.files(
  path = "data/products/nlcdMask",
  pattern = ".gpkg$", # ends in .shp
  full.names = TRUE,
  recursive = TRUE
)

#function that pulls in specific files based on year, reprojects, then combines to a mask layer 
year <- "2010"
nlcdPaths <- nlcdPaths
censusPaths <- censusPaths
crsLayer <- crs

combine_layers_by_year <- function(year, nlcdPaths, censusPaths, crsLayer) {
  print(year)
  # read and project 
  rAndP <- function(file, crsLayer = crsLayer){
    layer <- vect(file)
    f1 <- project(layer, crsLayer)
    return(f1)
  }
  # Filter files based on the year
  nlcd <- grep(pattern = year, x = nlcdPaths, value = TRUE) |>
    rAndP(crsLayer = crsLayer)
  # export 
  print("export nlcd")
  terra::writeVector(x = nlcd, 
                     filename = paste0("data/products/nlcdMask/forestProjected_",year,".gpkg"),
                     overwrite = TRUE)
  # names(nlcd) <- "value"
  census <- grep(pattern = year, x = censusPaths, value = TRUE) |>
    rAndP(crsLayer = crsLayer)
  # export
  print("export census")
  terra::writeVector(x = census, 
                     filename = paste0("data/products/urbanMask/urbanProjected_",year,".gpkg"),
                     overwrite = TRUE)

  
  # Combine all layers using union (assuming there are always two layers)
  # combined_layers <- union(nlcd, census) # this is taking a long time so avoiding for now... 
  # 
  # return(combined_layers)
}

years <- c("2010","2016","2020")
purrr::map(.x = years,combine_layers_by_year,
           nlcdPaths = nlcdPaths,
           censusPaths = censusPaths,
           crsLayer = crsLayer)


# Mask to sub grids  ------------------------------------------------------
mile2 <- terra::vect("data/products/modelGrids/two_sq_grid.gpkg")
forestMask <- terra::vect("data/products/nlcdMask/forestProjected_2016.gpkg")
townMask <- terra::vect("data/products/urbanMask/urbanProjected_2016.gpkg")
  
gridID <- "12659"
grids <- mile2
forest <- forestMask
town <- townMask
  
cropFeatures <- function(gridID, grids, forest,town){
  print(gridID)
  # select the grid 
  g1 <- grids[grids$FID_two_grid == gridID, ]
  # crop 1 
  f1 <- terra::crop(x = forest, y = g1)
  t1 <- terra::crop(x = town, y = g1)
  return(list(forest = f1, town = t1))
}
    
id2016 <- c("594","12659","11710","18037","6165","12781","11319","24110","17395")	

results <- purrr::map(.x = id2016, .f = cropFeatures,
                      grids = mile2, 
                      forest = forestMask,
                      town = townMask)





