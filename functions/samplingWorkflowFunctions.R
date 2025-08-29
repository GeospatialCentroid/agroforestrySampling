


# get spatial grids -------------------------------------------------------
getGrids <- function(gridSize,year = 2010){
  
  gridKM <- list.files("data/derived/spatialFiles",
                      pattern = ".gpkg",
                      full.names = TRUE)
  gridM <- list.files("data/products/modelGrids",
                      pattern = ".gpkg",
                      full.names = TRUE)
  # miles 
  if(gridSize == 2){
    grid <- terra::vect(gridM[grepl(pattern = "two_sq", x = gridM)])
  }
  if(gridSize == 12){
    grid <- terra::vect(gridM[grepl(pattern = year, x = gridM)])
  }
  # kilometers 
  if(gridSize == 10){
    grid <- terra::vect(gridKM[grepl(pattern = "grid10k", x = gridKM)])
  }
  if(gridSize == 5){
    grid <- terra::vect(gridKM[grepl(pattern = "grid5k", x = gridKM)])
    
  }
  if(gridSize == 25){
    grid <- terra::vect(gridKM[grepl(pattern = "grid25k", x = gridKM)])
  }
  return(grid)
}




# pulling in counts data --------------------------------------------------
get10kCSVs <- function(){
  k10 <- list.files("data/derived/areaCounts/10k", 
                    pattern = ".csv",
                    full.names = TRUE)
  return(k10)
}


getSpatialFiles <- function(area){
  # spatial data 
  f1 <- list.files("data/derived/spatialFiles",
                   full.names = TRUE)
  f2 <- f1[grepl(pattern = area, x = f1,ignore.case = TRUE)] |>
    terra::vect()
  return(f2)
}

# set subregion name "ECO", "MLRA", "LRR", "koppen"
getSubRegionName <- function(area){
  # Condition for gathering areas files 
  if(area == "ECO"){
    columnID <- "US_L3NAME"
  }
  if(area == "koppen"){
    # set column ID 
    columnID <- "ClimCode"
  }
  if(area == "LRR"){
    # set column ID 
    columnID <- "LRR_NAME"
  }
  if(area == "MLRA"){
    # set column ID 
    columnID <- "MLRA_ID"
  }
  return(columnID)
}




percentArea <- function(areaData){
  # filter csvs and generate summary measures 
  d1 <- areaData 
  if(length(d1)> 0){
    d1 <- d1 |>
      read_csv() 
    # group data so there is only one measure per unique ID 
    d2 <- d1 |>
      group_by(ID) |>
      summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)))|>
      dplyr::select(-NA.)
    # sumarize counts 
    d3 <- d2 |>
      rowwise() |>
      dplyr::mutate(
        cells2010 = sum(c_across(c(X1,X4,X6,X9)),na.rm=TRUE),
        cells2016 = sum(c_across(c(X3,X4,X8,X9)),na.rm=TRUE),
        cells2020 = sum(c_across(c(X5,X6,X8,X9)),na.rm=TRUE),
        totalCells = sum(c_across(X0:X9),na.rm=TRUE),
        percent10 = (cells2010 / totalCells)*100,
        percent16 = (cells2016 / totalCells)*100,
        percent20 = (cells2020 / totalCells)*100
      ) 
  }else{
    d3 <- data.frame(matrix(nrow = 0, ncol = 11))
    names(d3) <- c("ID","modelGrid","X0","X1","X3","X4","X5","X6","X8","X9","NA.") 
  }
  return(d3)
}

percentAreas <- function(name,areaData){
  # filter csvs and generate summary measures 
  d1 <- areaData[grepl(pattern = name, x = areaData)] 
  if(length(d1)> 0){
    d1 <- d1 |>
      read_csv() 
    # group data so there is only one measure per unique ID 
    d2 <- d1 |>
      group_by(ID) |>
      summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)))|>
      dplyr::select(-NA.)
    # sumarize counts 
    d3 <- d2 |>
      rowwise() |>
      dplyr::mutate(
        cells2010 = sum(c_across(c(X1,X4,X6,X9)),na.rm=TRUE),
        cells2016 = sum(c_across(c(X3,X4,X8,X9)),na.rm=TRUE),
        cells2020 = sum(c_across(c(X5,X6,X8,X9)),na.rm=TRUE),
        totalCells = sum(c_across(X0:X9),na.rm=TRUE),
        percent10 = (cells2010 / totalCells)*100,
        percent16 = (cells2016 / totalCells)*100,
        percent20 = (cells2020 / totalCells)*100
      ) 
  }else{
    d3 <- data.frame(matrix(nrow = 0, ncol = 11))
    names(d3) <- c("ID","modelGrid","X0","X1","X3","X4","X5","X6","X8","X9","NA.") 
  }
  return(d3)
}



stratSample <- function(areaType, i ,results){
  # run startified sample 
  # storage element 
  featureStorage <- data.frame(
    region = areaType,
    subRegion = i, 
    totalGrids = nrow(results),
    sampleN_2010 = NA,
    sampleN_2016 = NA,
    sampleN_2020 = NA
  )
  # get grid feature for stratitifed samples 
  index <- as.data.frame(area[,id]) |> pull() 
  subRegion <- terra::subset(x = area,index == i)
  selGrids <- terra::crop(grids, subRegion)
  thresVals <- list(
    "2010" = mean(results$percent10, na.rm = TRUE),
    "2016" = mean(results$percent16, na.rm = TRUE),
    "2020" = mean(results$percent20, na.rm = TRUE)
  )
  # construct results files 
  valsAllYears <- results |> 
    dplyr::select(ID,
                  percent2010 = "percent10",
                  percent2016 = "percent16",
                  percent2020 = "percent20")
  # loop over years 
  for(year in c("2010","2016","2020")){
    print(year)
    # select threshold based on year 
    thres <- thresVals[[year]]
    low <- thres -(thres *0.1)
    high <- thres + ( thres*0.1)
    # construct results files 
    yearIndex <- grepl(pattern = year, x = names(valsAllYears))[2:4]
    vals <- valsAllYears[,c(TRUE, yearIndex)]
    names(vals) <- c("ID","percent")
    # loop over all possibe rows 
    totalSam <- nrow(vals)
    if(totalSam > 100){
      Range <- c(seq(1,to=totalSam, by = 5), totalSam)
    }
    if(totalSam > 300){
      Range <- c(seq(1,to=totalSam, by = 10), totalSam)
    }
    if(totalSam <= 100){
      Range <- seq(1,totalSam, 1)
    }
    
    
    for(sampleN in Range){
      # repeat with different random seeds 
      for(j in 1:20){
        set.seed(j)
        # stratified sample 
        s1 <- terra::spatSample(x = selGrids,
                                size = sampleN,
                                method = "regular")
        # terra::plot(s1)
        # select and summarize  
        random_numbers <- sample(x = nrow(s1), size = sampleN, replace = FALSE)
        # select grid IDs 
        d1 <- vals |>
          dplyr::filter(ID %in% s1$ID )
        # filter
        d1<-d1[random_numbers,]
        # print(d1$ID)
        #summarize
        percentage <- mean(d1$percent, na.rm=TRUE)
        withinThres <- percentage <= high & percentage >= low
        
        # 
        d1 <- data.frame(percentage = percentage, withinThres = withinThres)
        # store results 
        if(j == 1){
          d2 <- d1
        }else{
          d2 <- dplyr::bind_rows(d2, d1)
        }
      } # end of j loop 
      # filter d2 to true only values 
      trueSamples <- d2 |> dplyr::filter(withinThres == TRUE)
      # 80% threshold 
      if(nrow(trueSamples) >=16){
        print(year)
        if(year == 2020){
          featureStorage$sampleN_2020 <- sampleN
        }
        if(year == 2016){
          featureStorage$sampleN_2016 <- sampleN
        }
        if(year == 2010){
          featureStorage$sampleN_2010 <- sampleN
        }
        break()
      }else{
        if(sampleN == nrow(vals)){
          if(year == 2020){
            featureStorage$sampleN_2020 <- sampleN
          }
          if(year == 2016){
            featureStorage$sampleN_2016 <- sampleN
          }
          if(year == 2010){
            featureStorage$sampleN_2010 <- sampleN
          }
          break()
        }
      }
    }
  }
  return(featureStorage)
}


# 12m functions getGeographicAreas -------------------------------------------------------------------------
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



# stratified sampling functions  ------------------------------------------


# functions --------------------------------------------------------------
## grids are ordered from bottom left - across and the back to bottom left and up
# sample selection  ------------------------------------------------------- 
stratifiedSelection <- function(grids, sampleNumber, seed = 1234){
  # get total number of grids 
  total <- nrow(grids)
  ids <- grids$sampleID
  # generate a random id selection
  start <- 20 #ids[sample(1:total, 1)]
  
  # if sampleNumber > 1, select additional points 
  if(sampleNumber > 1){
    # how far apart should samples be
    interval <- floor(total/sampleNumber)
    # develop a vector based on random start and interval 
    sel <- seq(from= start, by = interval, length.out = sampleNumber)
    # storage element 
    selected <- c()
    # generate the index based on the Malto function 
    for(i in 1:length(sel)){
      selected[i] <- sel[i] %% total
    }
  }else{
    # return the singular value 
    selected <- start
  }
  return(selected)
}

# Prep grids  -------------------------------------------------------------
prepGrids <- function(grids, subGeo){
  # crop to subgrid 
  g1 <- terra::crop(grids, subGeo)
  # reassign id  
  g1$sampleID <- 1:nrow(g1)
  # assign area 
  g1$area <- terra::expanse(g1, unit = "km")
  # pecent area for each feature -- not using directly 
  # g1$propArea <- g1$area/sum(g1$area, na.rm = TRUE)
  
  # return the values 
  return(g1)
}

pullAreaFiles <- function(featName, size){
  files <- list.files(paste0("data/products/areaSummaries/",size),
                      full.names = TRUE)
  # index from name 
  areas <- files[grepl(pattern = featName, files)]
  # spatial objects 
  files2 <- list.files("data/derived/spatialFiles", 
                       full.names = TRUE)
  s1 <- terra::vect(files2[grepl(pattern = featName, 
                                 x = files2, 
                                 ignore.case = TRUE)])
  # get subgrid ID 
  columnID <- getSubRegionName(featName)
  return(
    list(
      areaSummaries = areas,
      spatial = s1,
      name = featName,
      columnID = columnID
    )
  )
}

# weighted area calculation 
getWeightedTOF <- function(g2){
  totalAreaPop <- sum(g2$area)
  
  weightAreas <- data.frame(
    year = c("2010", "2016", "2020"),
    tofRatio = NA,
    tofArea = NA
  )
  
  # caculate ratio and area measures 
  r10 <- weighted.mean(x = g2$percent10, w = g2$area, na.rm = TRUE)
  a10 <- r10 * totalAreaPop
  r16 <- weighted.mean(x = g2$percent16, w = g2$area, na.rm = TRUE)
  a16 <- r16 * totalAreaPop
  r20 <- weighted.mean(x = g2$percent20, w = g2$area, na.rm = TRUE)
  a20 <- r20 * totalAreaPop
  # assign values 
  weightAreas$tofRatio <- c(r10,r16,r20)
  weightAreas$tofArea <- c(a10,a16,a20)
  # export 
  return(weightAreas)
}

# primary stratified sample method 
callStartitfied <- function(featName, size,proportionValue){
  # Get names for indexing area files 
  files <- pullAreaFiles(featName = featName, size = size)
  # sort 
  areaData <- files$areaSummaries
  s1 <- files$spatial
  name <- files$name
  # assign generic column ID 
  s1$genericID <- as.data.frame(s1)[,files$columnID]
  
  # run 
  for(feat in 1:nrow(s1)){
    print(feat)
    # define area object 
    spat <- s1[feat,]
    df_spat <- as.data.frame(spat)
    # id 
    id <- df_spat[,"genericID"] # this is going to change depending on datasource 
    # filter out select grids 
    g1 <- prepGrids(grids = grids, subGeo = spat)
    
    # calculate the total area 
    totalArea <- sum(g1$area, na.rm = TRUE)
    
    # grab area file and filter
    data <- areaData[grepl(pattern = id, x = areaData)] |> 
      read_csv() 
    |> 
      dplyr::filter(ID %in% g1$ID)|>
      dplyr::select(
        "ID", "percent10", "percent16", "percent20"
      ) |> 
      dplyr::mutate(
        "percent10" = percent10/100,
        "percent16" = percent16/100,
        "percent20" = percent20/100
      )
    # join to spatial object 
    g2 <- terra::merge(x = g1, y = data, by = "ID") |>
      as.data.frame()
    # calculate the total weight average per year 
    ## these are the values were trying to solve for our sampling 
    weightedTOF <- getWeightedTOF(g2)
    
    # assign new values to output object 
    # storage dataframe for output 
    output <- data.frame(
      id = id,
      totalAreas = nrow(g2),
      wTOF10 = weightedTOF$tofArea[1],
      wTOF16 = weightedTOF$tofArea[2],
      wTOF20 = weightedTOF$tofArea[3],
      sample10 = NA,
      averageTOF10 = NA,
      sample16 = NA,
      averageTOF16 = NA,
      sample20 = NA,
      averageTOF20 = NA
    )
    # loop over years 
    for(year in c("2010","2016","2020")){
      if(year == "2010"){
        g3 <- g2 |>
          dplyr::select(
            "ID","sampleID","area",
            tof = "percent10"
          )
        goalTOF <- weightedTOF |> 
          dplyr::filter(year == "2010") |>
          pull(tofArea)
      }
      if(year == "2016"){
        g3 <- g2 |>
          dplyr::select(
            "ID","sampleID","area",
            tof = "percent16"
          )
        goalTOF <- weightedTOF |> 
          dplyr::filter(year == "2016") |>
          pull(tofArea)
      }
      if(year == "2020"){
        g3 <- g2 |>
          dplyr::select(
            "ID","sampleID","area",
            tof = "percent20"
          )
        goalTOF <- weightedTOF |> 
          dplyr::filter(year == "2020") |>
          pull(tofArea)
      }
      
      # establish the acceptable range - changes based on year 
      tenPercent <- goalTOF *0.1
      low <- goalTOF - tenPercent
      high <- goalTOF + tenPercent
      
      # from here we do our sampling 
      for(i in 1:nrow(g3)){
        # itorate with random start 
        output2 <- data.frame(nSample = rep(i, 20),
                              estimatedTOF = NA)
        for(j in 1:20){
          set.seed(j)
          # pull a sample 
          sample <- sample_n(tbl = g3, size = i)
          # ratio estimator from the sample
          sampleRatio <- weighted.mean(x = sample$tof, w = sample$area, na.rm = TRUE)
          # TOF area estimate 
          tofEstimate <- sampleRatio * totalArea
          # store values  
          output2[j, "estimatedTOF"] <- tofEstimate
          
        }
        # exclude any duplications in the data 
        output2 <- distinct(output2) |>
          dplyr::mutate(
            inRange = case_when(
              estimatedTOF >= low & estimatedTOF <= high ~ TRUE,
              TRUE ~ FALSE
            )
          )
        # test to see if 80% of random draws are within threshold 
        average <- mean(output2$inRange) * 100
        if(average >= proportionValue){
          if(year == "2010"){
            output$sample10 <- i
            output$averageTOF10 <- mean(output2$estimatedTOF, na.rm = TRUE)
          }
          if(year == "2016"){
            output$sample16 <- i
            output$averageTOF16 <- mean(output2$estimatedTOF, na.rm = TRUE)
          }
          if(year == "2020"){
            output$sample20 <- i
            output$averageTOF20 <- mean(output2$estimatedTOF, na.rm = TRUE)
          }
          break()
        }
      }
    }
    # bind data to output dataframe 
    if(feat == 1){
      results <- output
    }else{
      results <- bind_rows(results,output)
    }
  }
  # return
  return(results)
}








