


# get spatial grids -------------------------------------------------------
getGrids <- function(year){
  #this might not be needed as the grid features are just used for indexing... 
  grids <- list.files("data/products/modelGrids",
                      pattern = ".gpkg",
                      full.names = TRUE)
  # g2 <- terra::vect(grids[grepl(pattern = "two_sq", x = grids)])
  grid <- terra::vect(grids[grepl(pattern = year, x = grids)])
  return(grid)
}






# getGeographicAreas -------------------------------------------------------------------------
getGeographicAreas <- function(areaType){
  # spatial data 
  f1 <- list.files("data/derived/spatialFiles",
                   full.names = TRUE)
  f2 <- f1[grepl(pattern = areaType, x = f1)]
  
  # Condition for gathering areas files 
  if(areaType == "eco"){
    areaFiles <- list.files(
      path = "data/derived/areaCounts/EPA_Level3",
      full.names = TRUE)
    # set column ID s
    columnID <- "US_L3NAME"
  }
  if(areaType == "Koppen"){
    areaFiles <- list.files(
      path = "data/derived/areaCounts/koppen_2020",
      full.names = TRUE)
    # set column ID 
    columnID <- "ClimCode"
  }
  if(areaType == "LRR"){
    areaFiles <- list.files(
      path = "data/derived/areaCounts/LRR",
      full.names = TRUE)
    # set column ID 
    columnID <- "LRRSYM"
  }
  if(areaType == "MLRA"){
    areaFiles <- list.files(
      path = "data/derived/areaCounts/MLRA",
      full.names = TRUE)
    # set column ID 
    columnID <- "MLRA_ID"
  }
  return(list(
    vect = f2, 
    files = areaFiles,
    columnID = columnID 
  ))
}






# select

# sub unit geographies ----------------------------------------------------
## process over unique areas within the selected geography 
subUnitSelection <- function(areaName, columnID, vect, files){
  # index sub region 
  vals <- as.data.frame(vect) |>
    dplyr::select(all_of(columnID))|> 
    pull()
  
  subRegion <- vect[vals == areaName, ]
  # files from the subregion 
  areaVals <- files[grepl(pattern = areaName, x = files)] |>
    readr::read_csv()|>
    dplyr::mutate(
      percent10 = (cells2010 / (newArea * 1000000))*100,
      percent16 = (cells2016 / (newArea * 1000000))*100,
      percent20 = (cells2020 / (newArea * 1000000))*100
    )
  # get true areas 
  # full area
  fullArea <- terra::expanse(x = subRegion, unit = "m")
  # percent tof
  total10 <- (sum(areaVals$cells2010)/sum(areaVals$newArea* 1000000))*100
  total16 <- (sum(areaVals$cells2016)/sum(areaVals$newArea* 1000000))*100
  total20 <- (sum(areaVals$cells2020)/sum(areaVals$newArea* 1000000))*100
  
  return(
   list(
     subRegion = subRegion,
     areaVals = areaVals,
     fullArea = fullArea, 
     total10 = total10,
     total16 = total16,
     total20 = total20
   ) 
  )
}



# random sample  ----------------------------------------------------------
## itorative method to determine the number of locations required for a percentile match 


runRandomSample <- function(subUnit){
  # storage element 
  featureStorage <- data.frame(
    area = as.data.frame(subUnit$subRegion)[, columnID],
    totalGrids = nrow(subUnit$areaVals),
    sampleN_2010 = NA,
    sampleN_2016 = NA,
    sampleN_2020 = NA
  )
  print(featureStorage$area)
  
  # loop over years 
  ## 80% caputre rate with 20 unique random draws 
  for(year in c(2010,2016,2020)){
    allVals <- subUnit$areaVals
    if(year == 2010){
      vals <- allVals[,c("newArea", "cells2010")]
      thres <- subUnit$total10
    }
    if(year == 2016){
      vals <- allVals[,c("newArea", "cells2016")]
      thres <- subUnit$total16
    }
    if(year == 2020){
      vals <- allVals[,c("newArea", "cells2020")]
      thres <- subUnit$total20
    }
    # set threshold 
    low <- thres -(thres *0.1)
    high <- thres + ( thres*0.1)
    names(vals) <- c( "newArea", "cells")
    for(i in 1:nrow(vals)){
      for(j in 1:20){
        set.seed(j)
        d1 <- vals |>
          dplyr::slice_sample(n = i) |>
          dplyr::summarise(
            # calculated the same method as threshold values
            percentage = (sum(cells) /(sum(newArea) * 1000000))*100)|>
          dplyr::mutate(
            withinThres = case_when(
              percentage <= high & percentage >= low ~TRUE,
              .default = FALSE
            )
          )
        if(j == 1){
          d2 <- d1
        }else{
          d2 <- dplyr::bind_rows(d2, d1)
        }
      }
      # filter d2 to true only values 
      trueSamples <- d2 |> dplyr::filter(withinThres == TRUE)
      # 80% threshold 
      if(nrow(trueSamples) >=16){
        print(year)
        if(year == 2020){
          featureStorage$sampleN_2020 <- i
        }
        if(year == 2016){
          featureStorage$sampleN_2016 <- i
        }
        if(year == 2010){
          featureStorage$sampleN_2010 <- i
        }
        break()
      }
    }
  }
  return(featureStorage)
}



# run stratified sample ---------------------------------------------------
runStratifiedSample <- function(subUnit, modelGrids){
  # storage element 
  featureStorage <- data.frame(
    area = as.data.frame(subUnit$subRegion)[, columnID],
    totalGrids = nrow(subUnit$areaVals),
    sampleN_2010 = NA,
    sampleN_2016 = NA,
    sampleN_2020 = NA
  )
  print(featureStorage$area)
  
  # loop over years 
  ## 80% caputre rate with 20 unique random draws 
  for(year in c(2010,2016,2020)){
    allVals <- subUnit$areaVals
    if(year == 2010){
      vals <- allVals[,c("Unique_ID", "newArea", "cells2010")]
      thres <- subUnit$total10
      # select the spatial object 
      modelGrid <- getGrids(year = "2010")
      
    }
    if(year == 2016){
      vals <- allVals[,c("Unique_ID","newArea", "cells2016")]
      thres <- subUnit$total16
      # select the spatial object 
      modelGrid <- getGrids(year = "2016")
    }
    if(year == 2020){
      vals <- allVals[,c("Unique_ID","newArea", "cells2020")]
      thres <- subUnit$total20
      # select the spatial object 
      modelGrid <- getGrids(year = "2020")

    }
    # filter model grids 
    includedGrids <- allVals$Unique_ID
    modelGrids <- terra::subset(modelGrid,modelGrid$Unique_ID %in% includedGrids)
    
    # set threshold 
    low <- thres -(thres *0.1)
    high <- thres + ( thres*0.1)
    names(vals) <- c("Unique_ID", "newArea", "cells")
    # get the spatial object 
    
    for(i in 1:nrow(vals)){
      for(j in 1:20){
        set.seed(j)
        
        # stratified sample 
        s1 <- terra::spatSample(x = modelGrids,
                                size = i,
                                method = "regular") |>
          terra::crop(subUnit$subRegion)|>
          as.data.frame()
        
        
        d1 <- vals |>
          dplyr::filter(Unique_ID %in% s1$Unique_ID )|>
          dplyr::slice_sample(n = i)|>
          dplyr::summarise(
            # calculated the same method as threshold values
            percentage = (sum(cells) /(sum(newArea) * 1000000))*100)|>
          dplyr::mutate(
            withinThres = case_when(
              percentage <= high & percentage >= low ~TRUE,
              .default = FALSE
            )
          )
        if(j == 1){
          d2 <- d1
        }else{
          d2 <- dplyr::bind_rows(d2, d1)
        }
      }
      # filter d2 to true only values 
      trueSamples <- d2 |> dplyr::filter(withinThres == TRUE)
      # 80% threshold 
      if(nrow(trueSamples) >=16){
        print(year)
        if(year == 2020){
          featureStorage$sampleN_2020 <- i
        }
        if(year == 2016){
          featureStorage$sampleN_2016 <- i
        }
        if(year == 2010){
          featureStorage$sampleN_2010 <- i
        }
        break()
      }
    }
  }
  return(featureStorage)
}


