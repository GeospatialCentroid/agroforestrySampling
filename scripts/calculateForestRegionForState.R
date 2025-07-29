##
# generalized method for calcuating the total area of trees outside of forest for a specific geographic area 
#
#
##

# load in library 
pacman::p_load(dplyr, terra, tictoc, readr, purrr, furrr, tictoc)

## load in the files locations 
allFiles <- list.files(path = "~/trueNAS/work/Agroforestry/data/products/changeOverTime",
                    full.names = TRUE,
                    pattern = "_2.tif")

# looking at 5 
g1 <- terra::vect("data/products/modelGrids/modelGrids_2020.gpkg")
g1$totalArea <- terra::expanse(x = g1, unit = "km")

# reworking method to generate area measures instead of pixel counts --------

file <- allFiles[grepl(pattern = "X12-155_c", allFiles)]

areasPerGrid <- function(file){
  # get name of the area 
  b1 <- basename(file) |> 
    stringr::str_split(pattern = "_")|>
    unlist()
  name <- b1[1]
  # export locaiton 
  path <- paste0("data/derived/areaCounts/fullState/",name,"_area.csv")
  if(!file.exists(path)){
    print(path)
    # read in cot 
    r1 <- terra::rast(file)
    # Get area measures 
    a1 <- terra::expanse(r1$ChangeOverTime, unit="km", byValue=TRUE, wide = TRUE)
    # assign grid ID 
    a1[1,1] <- name
    # export 
    write_csv(x = a1, file = path)
    rm(r1)
    gc()
  }
}

# run the process 
# 1:5 32 secs 
# tic()
# purrr::map(.x = allFiles[16], .f =areasPerGrid )
# toc()

# furrr
# 1:5 multicore, 5 cores  : 14 sec 
plan(strategy = "multicore", workers = 10)
tic()
furrr::future_map(.x = allFiles, .f = areasPerGrid)
toc()
# compline and format 

source("~/trueNAS/work/clearTempFiles.R")



# Compile datasets  -------------------------------------------------------
d1 <- list.files("data/derived/areaCounts/fullState",
                 pattern = "_area.csv",
                 full.names = TRUE) 
for(i in 1:length(d1)){
  dat <- read_csv(d1[i])
  if(i == 1){
    data <- dat
  }else{
    data <- dplyr::bind_rows(data, dat)
  }
}
# get area measures of grid file 
write_csv(data, "data/derived/areaCounts/fullState/summaryAllGrids_areakm.csv")

# join in for spatial area data 
g2 <- as.data.frame(g1) |>
  dplyr::select(Unique_ID, areaOfSpatialFeature = totalArea)
d2 <- read_csv("data/derived/areaCounts/fullState/summaryAllGrids_areakm.csv")|>
  dplyr::left_join(g2, by = c("layer" = "Unique_ID" )) |>
  dplyr::select(-totalArea)
# get area measures of grid file 
write_csv(d2, "data/derived/areaCounts/fullState/summaryAllGrids_areakm.csv")


# 
# fullStateArea <- function(file){
#   # get name of the area 
#   b1 <- basename(file) |> 
#     stringr::str_split(pattern = "_")|>
#     unlist()
#   name <- b1[1]
#   
#   path <- paste0("data/derived/areaCounts/fullState/",name,".csv")
#   if(!file.exists(path)){
#     print(path)
#     # read in cot 
#     r1 <- terra::rast(file)
#     # select cot
#     vals <- terra::values(r1$ChangeOverTime)[,1]
#     # set a df 
#     df <- data.frame(
#       grid = name,
#       cells2010 = NA,
#       cells2016 = NA,
#       cells2020 = NA
#     )
#     
#     # Calculate total pixel count for each year based on specific values
#     df[,"cells2010"] <- sum(vals[vals %in% c(1,4,6,9)], na.rm = TRUE)
#     df[,"cells2016"] <- sum(vals[vals %in% c(3,4,8,9)], na.rm = TRUE)
#     df[,"cells2020"] <- sum(vals[vals %in% c(5,6,8,9)], na.rm = TRUE)
#     
#     # Convert the grid cell to a data frame and export to CSV
#     readr::write_csv(x = df, file = path)
#     rm(r1)
#   }
#   gc()
# }
# 
# callWorkflow <- function(files, workers, type){
#   if(type == "sequential"){
#     plan(sequential)
#   }
#   if(type == "multicore"){
#     plan("multicore", workers = workers) 
#   }
#   if(type == "multisession"){
#     plan("future::multisession", workers = workers) 
#   }
#   
#   tic()
#   furrr::future_map(.x = files, .f = fullStateArea)
#   toc()
# }
# 
# # clear files before next run 
# removeFiles <- function(){
#   f1 <- list.files("data/derived/areaCounts/fullState",
#                    pattern = "^X",
#                    full.names = TRUE) 
#   file.remove(f1)
# }
# # get numerical value from toc message 
# getNumber <- function(v){
#   feat <- v$callback_msg |>
#     stringr::str_split(pattern = " ") |>
#     unlist()
#   return(feat[1])
# }
# 
# 
# # set parameters for the run tests  ---------------------------------------
# # for(i in c(8)){
# #   workers <- i 
# #   # initial state that test the some cores and output 
# #   if(i == 2){
# #     files <- allFiles[1:2*i]
# #   }else{
# #     double <- i *2
# #     files <- allFiles[1:double]
# #   }
# #   # storage DF 
# #   df <- data.frame(
# #     totalFiles = length(files),
# #     workers = workers,
# #     sequential = NA,
# #     multisession = NA,
# #     multicore = NA
# #   )
# #   
# #   ## sequential 
# #   print("sequential")
# #   tic()
# #   callWorkflow(files = files, 
# #                workers = workers,
# #                type = "sequential")
# #   v <- toc()
# #   df$sequential <- getNumber(v)
# #   # clean for next run 
# #   removeFiles()
# #   
# #   ## multisession  
# #   print("multisession")
# #   tic()
# #   callWorkflow(files = files, 
# #                workers = workers,
# #                type = "multisession")
# #   v1 <- toc()
# #   df$multisession <- getNumber(v1)
# #   # clean for next run 
# #   removeFiles()
# #   
# #   
# #   ## mutlicore 
# #   print("multicore")
# #   tic()
# #   callWorkflow(files = files, 
# #                workers = workers,
# #                type = "multicore")
# #   v2 <- toc()
# #   df$multicore <- getNumber(v2)
# #   # clean for next run 
# #   removeFiles()
# #   
# #   # export summary files 
# #   readr::write_csv(x = df, file = paste0(
# #     "data/derived/areaCounts/fullState/runtime_",
# #     df$totalFiles, "-files_", 
# #     df$workers, "-workers.csv"
# #   ))
# # }
# # 
# # # compile all the datasets together 
# # results <- list.files(
# #   path = "data/derived/areaCounts/fullState",
# #   pattern = "runtime_",
# #   full.names = TRUE) |>
# #   readr::read_csv()
# 
# 
# # run full process based on results from test  ----------------------------
# tic()
# callWorkflow(files = allFiles, workers = 8, type = "mutlisession")
# toc()
# 
# # combine all the results 
# 
# # compile all the datasets together 
# results <- list.files(
#   path = "data/derived/areaCounts/fullState",
#   pattern = "^X",
#   full.names = TRUE) |>
#   readr::read_csv() 
# 
# # add summaries 
# # total ; sum of columns 
# # average, standard deviation 
# 
# total_row <- results |> 
#   dplyr::summarise(
#     grid = "total",              # Or you could put a label like "All Regions"
#     cells2010 = sum(cells2010, na.rm = TRUE),  # Sum of the Sales column
#     cells2016 = sum(cells2016, na.rm = TRUE),
#     cells2020 = sum(cells2020, na.rm = TRUE)   # Sum of the Units column
#   )# Sum of the Units column
# 
# # bind data 
# results2 <- dplyr::bind_rows(results,total_row) |>
#   dplyr::rowwise() |>  # Treat each row as a group
#   dplyr::mutate(
#     mean = mean(c(cells2010,cells2016,cells2020), na.rm = TRUE),
#     standardDev = sd(c(cells2010,cells2016,cells2020), na.rm = TRUE),
#     coVariation = (standardDev/mean)*100
#   )
# # export 
# readr::write_csv(results2, "data/derived/areaCounts/fullState/summaryAreaMeasures.csv")
# 
# # #issue with mutliple features having the same values 
#  issues <- as.data.frame(results2) |>
#    dplyr::filter(cells2010 == 1072906)|>
#    dplyr::select(grid)|>
#    dplyr::pull()
#  
#  
# # 
# # # list files and delete issue locations 
# # files2 <-list.files(
# #   path = "data/derived/areaCounts/fullState",
# #   pattern = "^X",
# #   full.names = TRUE)
# # for(i in issues){
# #   print(i)
# #   f1 <- files2[grepl(pattern = paste0(i,".csv"), files2)]
# #   file.remove(f1)
# # }
# 
# 
# # export 
# readr::write_csv(x = results, file = "data/products/areaSummaries/fullState.csv")
