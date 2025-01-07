##
# generalized method for calcuating the total area of trees outside of forest for a specific geographic area 
#
#
##

# load in library 
pacman::p_load(dplyr, terra, tictoc, readr, purrr, furrr)


## load in the files locations 
files <- list.files(path = "~/trueNAS/work/Agroforestry/data/products/changeOverTime",
                    full.names = TRUE,
                    pattern = "_2.tif")

## load in the grid file --- just need the spatial information so the year doesn't matter. 
grids <- terra::vect("data/products/modelGrids_2010.gpkg")

## ecoregion measures 
### Some template code for reprojecting the ecoregion file 
# e1 <- terra::vect("data/raw/spatialAreaFiles/ecoregions/us_eco_l3.shp") |>
#   terra::project(grids)
# # export 
# terra::writeVector(x = e1, filename = "data/raw/spatialAreaFiles/ecoregions/us_eco_l3.gpkg")

# Load ecoregion data with the correct projection 
eco1 <- terra::vect("data/raw/spatialAreaFiles/ecoregions/us_eco_l3.gpkg")

# Crop ecoregions to the extent of the grids
ecoCrop <- terra::crop(x = eco1, y = grids)


## function for determining which locations are inside a geographic areas
# This function calculates the total area of trees outside of forests within 
# a specific geographic area (defined by 'spatialArea') for different years. 
# It iterates over grid cells, masks the raster data to the grid cell, 
# and calculates pixel counts for specific values representing tree cover.

# to get the total area per year we will have to read in the csv, bind them, then sum the values. 
# I choose this route because this is a lengthly processing step and I don't think I've captured all 
# the potnetial errors, so we're just writing out the intemediate results to not loose any work. 

# Arguments:
# spatialArea: A terra::vect object defining the geographic area of interest.
# grids: A terra::vect object containing the grid cells. Full state coverage is fine 
# areaName: A character string representing the name of the geographic area type (e.g., "ecoRegionLevel3").
# specificName: A character string representing the specific name of the geographic area (e.g., the ecoregion name). 
# require because some grid will be present in multiple ecoregions/mlras
# files: A character vector of file paths to raster data.

testGridIntersection <- function(spatialArea, grids, areaName, specificName, files){
  
  # Create a folder to store the output
  folder <- paste0("data/derived/areaCounts/", areaName)
  if(!dir.exists(folder)){
    dir.create(folder)
  }
  
  # Intersect the grids with the area of interest
  g2 <- terra::intersect(x = grids, y = spatialArea)
  
  # Calculate the area of the intersected grids
  g2$newArea <-  round(terra::expanse(g2, unit = "km"), digits = 2)
  
  # Select relevant columns from the intersected grids
  g2 <- g2[c("Unique_ID","originalArea","newArea" ), ] 
  
  # Check if the original and new areas are the same
  g2$sameArea <- g2$originalArea == g2$newArea
  
  # Initialize columns for pixel counts in different years
  g2$cells2010 <- NA
  g2$cells2016 <- NA
  g2$cells2020 <- NA
  
  # Iterate over the sub grids to calculate areas 
  for(j in 1:nrow(g2)){
    grid <- g2[j, ]
    id <- grid$Unique_ID
    # this print statement will not appear
    print(paste0("generating grid ", j, " out of ", nrow(g2)))
    
    # Define the output file path
    path <- paste0(folder,"/pixelCounts_",id,"_", specificName,"_.csv")
    # test for the presence of the file before starting the process 
    if(file.exist(path)){
      next()
    }else{
      # Select the raster file corresponding to the current grid ID
      test1 <- grepl(pattern = paste0(id,"_"), x = files)
      
      # Skip if the raster file is not found (to handle grids 1:8)
      if(!TRUE %in% test1){
        next()
      } else {
        r1 <- terra::rast(files[test1])
        
        # Mask the raster to the grid cell if the areas are not the same
        if(grid$sameArea == FALSE){
          r1 <- terra::mask(r1, grid)
        }
        
        # Calculate pixel counts for each value in the raster
        vals <- freq(r1)
        rm(r1) 
        
        # Calculate total pixel count for each year based on specific values
        grid[,"cells2010"] <- sum(vals[vals$value %in% c(1,4,6,9),"count"], na.rm = TRUE)
        grid[,"cells2016"] <- sum(vals[vals$value %in% c(3,4,8,9),"count"], na.rm = TRUE)
        grid[,"cells2020"] <- sum(vals[vals$value %in% c(5,6,8,9),"count"], na.rm = TRUE)
        
        # Convert the grid cell to a data frame and export to CSV
        df <- as.data.frame(grid)
        readr::write_csv(x = df, file = path)
      }
    }
  }
}

# Apply the function to the first ecoregion as a test
areaName <-"ecoRegionLevel3"
specificName <- ecoCrop[1, ]$US_L3NAME 
testGridIntersection(spatialArea = ecoCrop[1, ],
                     grids = grids,
                     areaName = areaName,
                     specificName = specificName,
                     files = files)


# Split the ecoregions into a list of individual ecoregion objects
ecos <- ecoCrop |> terra::split("US_L3NAME" )

# Apply the function to each ecoregion using a for loop
for(i in 1:length(ecos)){
  testGridIntersection(spatialArea = ecos[[i]],
                       grids = grids,
                       areaName = areaName,
                       specificName = ecos[[i]]$US_L3NAME, # Use the specific name from the ecoregion object
                       files = files)
}





# not working yet...  -----------------------------------------------------
# to use the purrr or furrr options we will need to have to adapt the function 
# to take in two changing variables. ecos and specificName, 
# easiest change is probably just applyng the map2 and passing a second list of eco names. 
# can pursue if important. 

# Apply the function to each ecoregion using purrr::map
### 
# purrr::map(.x = ecos, .f = testGridIntersection, 
#            grids = grids,
#            areaName = areaName,
#            specificName = specificName, 
#            files = files)

# Apply the function to each ecoregion using furrr::future_map (for parallel processing)
# Note: This requires setting up a parallel processing plan (e.g., plan(multicore))
# using about 40gb of ram, but I'm not 100% just howmany cores were functioning at once... 

# plan(multicore, workers = 4) 
# furrr::future_map(.x = ecos,  .f = testGridIntersection, 
#                    grids = grids,
#                    areaName = areaName,
#                    specificName = specificName, 
#                    files = files
# )