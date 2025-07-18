
  
pacman::p_load("terra", "dplyr", "readr", "sf", "tictoc", "tmap")

source("functions/samplingWorkflowFunctions.R")
# goal 
## want to generate a systematic sample of a AOI 
## these are iterative process that will continue until 
## 90% of the draws at a specific number result in value within +/- 10% of the know value 


# levels of analysis 
## geographic stratification  (ecoregions, koppen climate, lrr, mlra)

# default inputs  ---------------------------------------------------------
## 12m grids 
grids <- getGrids(gridSize = 10)
grid12 <- getGrids(gridSize = 12)

# Gather and Summarize area data -----------------------------------------------
data10k <- get10kCSVs()

runStratified <- function(areaType, data10k){
  print(areaType)
  # get spatial file 
  area <- getSpatialFiles(area = areaType)
  # filter tot area type 
  areaData <- data10k[grepl(pattern = areaType, x = data10k)]
  # set subregions 
  id <- getSubRegionName(areaType)
  allNames <- unique(area[,id]) |> as.data.frame() |> pull()
  # Gather complete stats for area 
  for(i in allNames){
    print(i)
    exportPath <- paste0("data/products/areaSummaries/10k/percentArea_",areaType,
                         "_",i,".csv" )
    if(!file.exists(exportPath)){
      print(i)
      results <- percentAreas(name = i, areaData = areaData)
      write_csv(results,exportPath)
    }else{
      results <- read_csv(exportPath)
    }
    exportPath2 <- paste0("data/derived/areaCounts/stratified10k_",areaType,"_",i,".csv")
    if(!file.exists(exportPath2)){
      if(nrow(results)>0){
        ssResults <- try(stratSample(areaType = areaType, i = i, results = results))
        if(class(ssResults) != "try-error"){
          write.csv(x = ssResults,file = exportPath2)  
        }
      }
    }
  }
  # #compile inputs
  # files <- list.files("data/derived/areaCounts",
  #                     pattern = "10k",
  #                     full.names = TRUE)
  # # filter to region 
  # f2 <- files[grepl(files, pattern = paste0("stratified10k_", areaType))] |>
  #   read_csv()
  # # export 
  # write_csv("data/derived/areaCounts/stratified_10k_",areaType,"all.csv")
}


runStratified(areaType = "MLRA", data10k = data10k)
# 
# # for(areaType in c( "MLRA", "LRR", "koppen","ECO")){
#   print(areaType)
#   # get spatial file 
#   area <- getSpatialFiles(area = areaType)
#   # filter tot area type 
#   areaData <- data10k[grepl(pattern = areaType, x = data10k)]
#   # set subregions 
#   id <- getSubRegionName(areaType)
#   allNames <- unique(area[,id]) |> as.data.frame() |> pull()
#   # Gather complete stats for area 
#   for(i in allNames){
#     print(i)
#     exportPath <- paste0("data/products/areaSummaries/10k/percentArea_",areaType,
#                          "_",i,".csv" )
#     if(!file.exists(exportPath)){
#       print(i)
#       results <- percentAreas(name = i, areaData = areaData)
#       write_csv(results,exportPath)
#     }else{
#       results <- read_csv(exportPath)
#     }
#     exportPath2 <- paste0("data/derived/areaCounts/stratified10k_",areaType,"_",i,".csv")
#     if(!file.exists(exportPath2)){
#       if(nrow(results)>0){
#         ssResults <- stratSample(areaType = areaType, i = i, results = results)
#         write.csv(x = ssResults,file = exportPath2)
#       }
#     }
#   }
#   #compile inputs
#   files <- list.files("data/derived/areaCounts",
#                       pattern = "10k",
#                       full.names = TRUE)
#   # filter to region 
#   f2 <- files[grepl(files, pattern = paste0("stratified10k_", areaType))] |>
#     read_csv()
#   # 
#   
# } 



# generate the stratified sample  -----------------------------------------
# run stratified sample ---------------------------------------------------
# 
# runStratifiedSample <- function(subUnit, modelGrids){
#   # storage element 
#   featureStorage <- data.frame(
#     area = as.data.frame(subUnit$subRegion)[, columnID],
#     totalGrids = nrow(subUnit$areaVals),
#     sampleN_2010 = NA,
#     sampleN_2016 = NA,
#     sampleN_2020 = NA
#   )
#   print(featureStorage$area)
#   
#   # loop over years 
#   ## 80% caputre rate with 20 unique random draws 
#   for(year in c(2010,2016,2020)){
#     allVals <- subUnit$areaVals
#     if(year == 2010){
#       vals <- allVals[,c("Unique_ID", "newArea", "cells2010")]
#       thres <- subUnit$total10
#       # select the spatial object 
#       modelGrid <- getGrids(year = "2010")
#       
#     }
#     if(year == 2016){
#       vals <- allVals[,c("Unique_ID","newArea", "cells2016")]
#       thres <- subUnit$total16
#       # select the spatial object 
#       modelGrid <- getGrids(year = "2016")
#     }
#     if(year == 2020){
#       vals <- allVals[,c("Unique_ID","newArea", "cells2020")]
#       thres <- subUnit$total20
#       # select the spatial object 
#       modelGrid <- getGrids(year = "2020")
#       
#     }
#     # filter model grids 
#     includedGrids <- allVals$Unique_ID
#     modelGrids <- terra::subset(modelGrid,modelGrid$Unique_ID %in% includedGrids)
#     
#     # set threshold 
#     low <- thres -(thres *0.1)
#     high <- thres + ( thres*0.1)
#     names(vals) <- c("Unique_ID", "newArea", "cells")
#     # get the spatial object 
#     
#     for(i in 1:nrow(vals)){
#       for(j in 1:20){
#         set.seed(j)
#         
#         # stratified sample 
#         s1 <- terra::spatSample(x = modelGrids,
#                                 size = i,
#                                 method = "regular") |>
#           terra::crop(subUnit$subRegion)|>
#           as.data.frame()
#         
#         
#         d1 <- vals |>
#           dplyr::filter(Unique_ID %in% s1$Unique_ID )|>
#           dplyr::slice_sample(n = i)|>
#           dplyr::summarise(
#             # calculated the same method as threshold values
#             percentage = (sum(cells) /(sum(newArea) * 1000000))*100)|>
#           dplyr::mutate(
#             withinThres = case_when(
#               percentage <= high & percentage >= low ~TRUE,
#               .default = FALSE
#             )
#           )
#         if(j == 1){
#           d2 <- d1
#         }else{
#           d2 <- dplyr::bind_rows(d2, d1)
#         }
#       }
#       # filter d2 to true only values 
#       trueSamples <- d2 |> dplyr::filter(withinThres == TRUE)
#       # 80% threshold 
#       if(nrow(trueSamples) >=16){
#         print(year)
#         if(year == 2020){
#           featureStorage$sampleN_2020 <- i
#         }
#         if(year == 2016){
#           featureStorage$sampleN_2016 <- i
#         }
#         if(year == 2010){
#           featureStorage$sampleN_2010 <- i
#         }
#         break()
#       }
#     }
#   }
#   return(featureStorage)
# }
# 
# 
# 
# 
# 
# 
# 
# 
# # 12m workflow ------------------------------------------------------------
# 
# 
# for(areaType in c("eco", "MLRA", "LRR", "Koppen")){
#   # pull area specific info 
#   geoPaths <- getGeographicAreas(areaType = areaType)
#   columnID <- geoPaths$columnID
#   
#   vect <- geoPaths$vect |> terra::vect() |> terra::aggregate(by = columnID)
#   files <- geoPaths$files
#   allNames <- unique(vect[,columnID]) |>
#     as.data.frame() |> 
#     pull()
#   
#   ## sub units within each stratification 
#   subUnits <- purrr::map(.x = allNames, 
#                          .f = subUnitSelection,
#                          columnID = columnID,
#                          vect = vect,
#                          files = files)
#   
#   
#   
#   
#   # Run Random Sampling  ----------------------------------------------------
#   ## for each year 
#   ## set threshold range 
#   ## sample N values and average results 
#   ## repeat 20 times 
#   ## test to see if 18 of the twenty samples fall within the range, then stop 
#   
#   plan(strategy = "multicore", workers = 12)
#   rand2 <- furrr::future_map(.x = subUnits, .f = runStratifiedSample) |>
#     dplyr::bind_rows()
#   write.csv(x = rand2,file =  paste0("data/derived/areaCounts/stratified_",areaType,".csv"))
# }
# 
# 
# 
# 
# 



