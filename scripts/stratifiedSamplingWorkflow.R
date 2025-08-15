
  
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
  # decide areas 
  g1$areas <- as.numeric(terra::expanse(x = g1, unit = "km"))
  totalArea <- sum(g1$areas)
  g1$totalAreaOverIndividualArea <- totalArea/g1$areas
  g1$individualAreaOverTotalArea <- g1$areas/totalArea
  
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

# stratified workflow  ----------------------------------------------------
grids <- getGrids(gridSize = 10)
mlra <- terra::vect("data/derived/spatialFiles/CONUS_MLRA_52_dissolved.gpkg")
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
  
  # test sample 
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
