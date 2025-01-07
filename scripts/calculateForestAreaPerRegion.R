##
# generalized method for calcuating the total area of trees outside of forest for a specific geographic area 
#
#
##


pacman::p_load(dplyr, terra, tictoc, readr, purrr, furrr)
source("functions/reclassByYear.R")

## load in the files locations 
files <- list.files(path = "~/trueNAS/work/Agroforestry/data/products/changeOverTime",
                    full.names = TRUE,
                    pattern = "_2.tif")
## load in the grid file 
grids <- terra::vect("data/products/modelGrids_2010.gpkg")

## ecoregion measures 
# e1 <- terra::vect("data/raw/spatialAreaFiles/ecoregions/us_eco_l3.shp") |>
#   terra::project(grids)
# # export 
# terra::writeVector(x = e1, filename = "data/raw/spatialAreaFiles/ecoregions/us_eco_l3.gpkg")
eco1 <- terra::vect("data/raw/spatialAreaFiles/ecoregions/us_eco_l3.gpkg")
ecoCrop <- terra::crop(x = eco1, y = grids)

uniqueEco <- ecoCrop$US_L3CODE

## function for determining which locations are inside a geographic areas
grids <- grids
spatialArea <- ecoCrop[1, ]
files <- files
areaName <-"ecoRegionLevel3"
specificName <- spatialArea$US_L3NAME ## needs to be defined 
# calculate original grid area 
grids$originalArea <- round(terra::expanse(grids, unit = "km"), digits = 2)

testGridIntersection <- function(spatialArea,grids,areaName,specificName, files){
  folder <- paste0("data/derived/areaCounts/",areaName)
  if(!dir.exists(folder)){
    dir.create(folder)
  }
  # area name 
  
  # intersect the grids to the area of interest  
  g2 <- terra::intersect(x = grids, y = spatialArea)
  g2$newArea <-  round(terra::expanse(g2, unit = "km"), digits = 2)
  # select elements of interest 
  g2 <- g2[c("Unique_ID","modelGrid","originalArea","newArea" ), ] 
  g2$sameArea <- g2$originalArea == g2$newArea
  
  # define empty values for total areas 
  g2$cells2010 <- NA
  g2$cells2016 <- NA
  g2$cells2020 <- NA
  # itorate over the sub grids to caculate areas 
  for(j in 1:nrow(g2)){
    grid <- g2[j, ]
    id <- grid$Unique_ID
    print(paste0("generateing grid ", j, " out of ", nrow(g2)))
    # test for existing output file 
    
    # export file 
    path <- paste0(folder,"/pixelCounts_",id,"_", specificName,"_.csv")
    
    # select raster file 
    test1 <- grepl(pattern = paste0(id,"_"), x = files)
    # test for if the file it present or not.. to catch grids 1:8
    if(!TRUE %in% test1){
      next()
    }else{
      r1 <- terra::rast(files[test1])
      # test to see if the object needs to be cropped. 
      if(grid$sameArea == FALSE){
        # crop the area of interest 
        r1 <- terra::mask(r1, grid)
      }
      # develop a frequency table -- pixel counts per value  
      vals <- freq(r1)
      rm(r1)
      # get the total pixel county by year 
      grid[,"cells2010"] <- sum(vals[vals$value %in% c(1,4,6,9),"count"], na.rm = TRUE)
      grid[,"cells2016"] <- sum(vals[vals$value %in% c(3,4,8,9),"count"], na.rm = TRUE)
      grid[,"cells2020"] <- sum(vals[vals$value %in% c(5,6,8,9),"count"], na.rm = TRUE)
      # export option
      ## at least while testing I don't expect this to complete everytime 
      df <- as.data.frame(grid)
      readr::write_csv(x = df, file = path)
      }
  }
}

# single area applied 
testGridIntersection(spatialArea = spatialArea,
                     grids = grids,
                     areaName = areaName,
                     specificName = specificName,
                     files = files)


# split out the ecocrop area into a series of objects 
ecos <- ecoCrop |> terra::split("US_L3NAME" )


# for loop 
for(i in 1:length(ecos)){
  testGridIntersection(spatialArea = ecos[[i]],
                       grids = grids,
                       areaName = areaName,
                       specificName = specificName,
                       files = files)
}


# purrr 
purrr::map(.x = ecos, .f = testGridIntersection, 
           grids = grids,
           areaName = areaName,
           specificName = specificName,
           files = files)

# furrr 
## setting the multisession 
### getting about 16gb of ram usage for single core 
plan(multicore, workers = 4) # can't run multicore without using terra::wrap/ terra::unwrap to pack the object to the multiple sessions 
### this stuff is never super stable but might be worth exploring depending on the run times of the sequential work. 
furrr::future_map(.x = ecos,  .f = testGridIntersection, 
                  grids = grids,
                  areaName = areaName,
                  specificName = specificName,
                  files = files
                  )
