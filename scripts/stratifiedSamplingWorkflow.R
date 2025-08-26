
  
pacman::p_load("terra", "dplyr", "readr", "sf", "tictoc", "tmap")
tmap_mode("view")
source("functions/samplingWorkflowFunctions.R")
# goal 
## want to generate a systematic sample of a AOI 
## these are iterative process that will continue until 
## 90% of the draws at a specific number result in value within +/- 10% of the know value 


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
  # pecent area for each feature 
  g1$propArea <- g1$area/sum(g1$area, na.rm = TRUE)
  
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
getWeightedTOF <- function(g1){
  weightAreas <- data.frame(
    year = c("2010", "2016", "2020"),
    tof = NA
  )
  weightAreas[1,2] <- weighted.mean(x = g1$percent10, w = g1$propArea)
  weightAreas[2,2] <- weighted.mean(x = g1$percent16, w = g1$propArea)
  weightAreas[3,2] <- weighted.mean(x = g1$percent20, w = g1$propArea)
  return(weightAreas)
}

# stratified workflow  ----------------------------------------------------
grids <- getGrids(gridSize = 10)
# mlra <- terra::vect("data/derived/spatialFiles/CONUS_MLRA_52_dissolved.gpkg")
featName <- "MLRA"
size <- "10k"


# Get names for indexing area files 
files <- pullAreaFiles(featName = featName, size = size)
# sort 
areaData <- files$areaSummaries
s1 <- files$spatial
name <- files$name
# subGeo <- s1[4,]
# get subgrid id 

index <- 2


# start with the MLRA object 
# for each area in the MLRA object 



for(feat in 1:nrow(s1)){
  print(feat)
  # define area object 
  spat <- s1[feat,]
  df_spat <- as.data.frame(spat)
  # id 
  id <- df_spat[,"MLRA_ID"] # this is going to change depending on datasource 
  # filter out select grids 
  g1 <- prepGrids(grids = grids, subGeo = spat)
  # calculate the total area 
  totalArea <- sum(g1$area, na.rm = TRUE)
  
  # grab area file and filter
  data <- areaData[grepl(pattern = id, x = areaData)] |> 
    read_csv() |> 
    dplyr::filter(ID %in% g1$ID)|>
    dplyr::select(
      "ID", "percent10", "percent16", "percent20"
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
    wTOF10 = weightedTOF$tof[1],
    wTOF16 = weightedTOF$tof[2],
    wTOF20 = weightedTOF$tof[3],
    sample10 = NA,
    sample16 = NA,
    sample20 = NA
  )
  # loop over years 
  for(year in c("2010","2016","2020")){
    if(year == "2010"){
      g3 <- g2 |>
        dplyr::select(
          "ID","sampleID","area","propArea",
          tof = "percent10"
        )
      goalTOF <- weightedTOF[1, 2]
    }
    if(year == "2016"){
      g3 <- g2 |>
        dplyr::select(
          "ID","sampleID","area","propArea",
          tof = "percent16"
        )
      goalTOF <- weightedTOF[2, 2]
    }
    if(year == "2020"){
      g3 <- g2 |>
        dplyr::select(
          "ID","sampleID","area","propArea",
          tof = "percent20"
        )
      goalTOF <- weightedTOF[3, 2]
    }
  
    # establish the acceptable range - changes based on year 
    tenPercent <- goalTOF *0.1
    low <- goalTOF - tenPercent
    high <- goalTOF + tenPercent

    # from here we do our sampling 
    for(i in 1:nrow(g3)){
      # itorate with random start 
      output2 <- data.frame(nSample = rep(i, 20),
                            averageTOF = NA,
                            averageTOF_overpop = NA)
      for(j in 1:20){
        set.seed(j)
        # pull a sample 
        sample <- sample_n(tbl = g3, size = i)
        # total area of sample 
        totalAreaSample <- sum(sample$area)
        # average area of sample 
        aveAreaSample <- totalAreaSample / nrow(sample)
        # proportionality factor 
        proFactor <- totalArea / totalAreaSample
        
        
        # calculate the proportion weight TOF 
        sample <- sample |>
          dplyr::mutate(
            relativeWeight = area/aveAreaSample, 
            weightedArea = aveAreaSample * relativeWeight * proFactor,
            weightTOF = tof * relativeWeight * proFactor
          )
        # store values  
        output2[j, "averageTOF"] <- mean(sample$weightTOF, na.rm =TRUE)
        output2[j, "averageTOF_overpop"] <- sum(sample$weightTOF, na.rm =TRUE)/output$totalAreas
        
      }
      # exclude any duplications in the data 
      output2 <- distinct(output2) |>
        dplyr::mutate(
          inRange = case_when(
            averageTOF_overpop >= low & averageTOF_overpop <= high ~ TRUE,
            TRUE ~ FALSE
          )
        )
      # test to see if 80% of random draws are within threshold 
      average <- mean(output2$inRange) * 100
      if(average >= 80){
        if(year == "2010"){
          output$sample10 <- i
        }
        if(year == "2016"){
          output$sample16 <- i
        }
        if(year == "2020"){
          output$sample20 <- i
        }
        next()
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








runStratified <- function(index,grids,files){
  # unpack files object 
  areaData <- files$areaSummaries
  spatial <- files$spatial
  name <- files$name
  columnID <- files$columnID
  # select sub geo 
  subGeo <- spatial[index,] 
  # define subgeoid 
  subGeoID <- as.data.frame(subGeo)[,columnID]
  # select area files 
  d1 <- areaData[grepl(pattern = subGeoID, x = areaData) ]  |>
    readr::read_csv() 
  
  # process grids 
  g1 <- prepGrids(grids = grids, subGeo = subGeo)
  g1$area <- terra::expanse(g1, unit = "km")
  
  
  # runSample 
  spat <- g1
  nGrids <- 5
  weightedArea <- function(spat, nGrids){
    df <- as.data.frame(spat)
    # total Area
    totalArea <- sum(g1$area)
    # generate sample 
    sample <- sample_n(tbl = df, size = nGrids)
    # total area of sample 
    totalAreaSample <- sum(sample$area)
    # average area of sample 
    aveAreaSample <- totalAreaSample / nrow(sample)
    # proportionality factor 
    proFactor <- totalArea / totalAreaSample
    # assign values 
    sample <- sample |>
      dplyr::mutate(
        relativeWeight = area/aveAreaSample, 
        weightedArea = aveAreaSample * relativeWeight * proFactor,
        weightTOF = TOF * relativeWeight * proFactor
      )
  }
  
  # pull a sample 
 
  
 
  
  # proportionality factor 
  proFactor <- totalAreaPop / totalAreaSample
  
  # assign values 
  ## one of the main issue I was having in the past was using the unique area data, rather than the aveAreaSample
  ## in the calculation of the weigthed area measures 
  ## assumign that this is not required of the TOF because were using a independent measure that is not area 
  sample <- sample |>
    dplyr::mutate(
      relativeWeight = area/aveAreaSample, 
      weightedArea = aveAreaSample * relativeWeight * proFactor,
      weightTOF = TOF * relativeWeight * proFactor
    )
  
  # check the area measure 
  print(paste("Area population:", sum(df$area)))
  print(paste("Weighted area of sample", sum(sample$weightedArea)))
  sum(df$area) == sum(sample$weightedArea)
  
  # check the TOF calculations 
  df$prop <- df$area / sum(df$area)
  # this weighted mean calculation uses the % area of a specific region against the whole as the weight 
  weightMeanTOF <- weighted.mean(x = df$TOF, w = df$prop)
  
  # interestingly the average weighed area requires that you using the population of the denominator in the calculation 
  averageWeightedTOF <-  sum(sample$weightTOF) / nrow(df)
  print(paste("Weighted Mean TOF population:", weightMeanTOF))
  print(paste("Weighted Mean TOF sample", averageWeightedTOF))
  # exact match only when nrow sample == nrow population 
  print(weightMeanTOF == averageWeightedTOF)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # sample 
  sample <- g1[stratifiedSelection(grids = g1, sampleNumber = 5), ]
  
  sample$areas <- terra::expanse(sample, unit = "km")
  
  
  
  terra::plot(subGeo)   
  terra::plot(g1, add = TRUE )
  terra::plot(sample, add = TRUE, col = "blue")
  # return(sample)
  
  # select the TOF for specific sample grids 
  d2 <- d1 |> 
    dplyr::filter(ID %in% sample$ID)|>
    dplyr::select("ID","percent10","percent16","percent20")|>
    dplyr::mutate("percent10" = percent10/100,
                  "percent16" = percent16/100,
                  "percent20" = percent20/100)
  # drop the spatial data and join the percent measures 
  s1 <- as.data.frame(sample) |>
    dplyr::left_join(d2, by ="ID") |>
    dplyr::select(-totalAreaOverIndividualArea, -individualAreaOverTotalArea)
  
  # area of the full object
  totalArea <- sum(g1$areas)
  # area of the sample 
  sampleArea <- sum(s1$areas)
  # average sampled area
  sampleAreaAverage <- sampleArea / nrow(s1)
  # whole area weight 
  fullAreaWeight <- totalArea/sampleArea

  # calculate out the TOF pre year 
  s2 <- s1 |>
    dplyr::mutate(
      sampleAreaProportion = areas / sampleAreaAverage,
      measuredWeight = sampleAreaProportion * sampleAreaAverage * fullAreaWeight,
      weightedTOF_10 = measuredWeight * percent10,
      weightedTOF_16 = measuredWeight * percent16,
      weightedTOF_20 = measuredWeight * percent20
    )
  
  # for each sample 
  ## run a mutate to determine the proportional value area/ average area * average weight * total area 
  s2 <- sample |>
    as.data.frame() |> 
    dplyr::select(-totalAreaOverIndividualArea, -individualAreaOverTotalArea)|>
    dplyr::mutate(
      averageArea = sampleAreaAverage, 
      wholeAreaWeight = wholeAreaWeight,
      sampleAreaProportion = areas / sampleAreaAverage,
      measuredWeight = sampleAreaProportion *sampleAreaAverage * fullAreaWeight
    )
  # total area check 
  sum(s2$measuredWeight) == totalArea
  
  
  
  
  
  ### second attempt based on gabriels notes 
  # totalAreaOfPopulation <- sum(g1$areas)
  # # average area of sampled locations 
  # fullAreaAverage <- totalAreaOfPopulation/nrow(g1)
  # #calculate the sample area 
  # sampleArea <- sum(sample$areas)
  # # calculate the full area weight 
  # fullAreaWeight <- totalAreaOfPopulation/sampleArea
  # # calculate the proportion of each sample location relative to the full area
  # s2 <- sample |>
  #   as.data.frame() |> 
  #   dplyr::select(-totalAreaOverIndividualArea, -individualAreaOverTotalArea)|>
  #   dplyr::mutate(
  #     sampleAreaProportion = areas / fullAreaAverage,
  #     measuredWeight = sampleAreaProportion * sampleArea * fullAreaWeight *
  #   )
  # 
  # 
  
  # Select files of interest from sample 
  allTOF_Data <- d1 |>
    dplyr::filter(ID %in% g1$ID) |>
    dplyr::select("ID","cells2010","cells2016","cells2020", "totalCells","percent10","percent16","percent20")
  # 
  sampleTOF_Data <- allTOF_Data |>
    dplyr::filter(ID %in% s2$ID)
}


sampledArea <- runStratified(index = 2, grids = grids, spatial = subGeo, files = files)
