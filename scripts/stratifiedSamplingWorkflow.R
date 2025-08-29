
  
pacman::p_load("terra", "dplyr", "readr", "sf", "tictoc", "tmap")
tmap_mode("view")
source("functions/samplingWorkflowFunctions.R")
# goal 
## want to generate a systematic sample of a AOI 
## these are iterative process that will continue until 
## 80% of the draws at a specific number result in value within +/- 10% of the know value 


# stratified workflow  ----------------------------------------------------
grids <- getGrids(gridSize = 10)

# set the potential areas 
featureAreas <- c("MLRA", "ECO","koppen", "LRR")

featName <- "MLRA"
size <- "10k"


# results at 80% agreement  ------------------------------------------------
mlra80 <- purrr::map(.x = featureAreas[1], .f = callStartitfied, size = "10k", proportionValue =  80, .progress = TRUE)
# eco <- purrr::map(.x = featureAreas[2], .f = callStartitfied, size = "10k", proportionValue =  80, .progress = TRUE) # error on element 3
# koppen <- purrr::map(.x = featureAreas[3], .f = callStartitfied, size = "10k", proportionValue =  80, .progress = TRUE) # error on element 3
LRR <- purrr::map(.x = featureAreas[4], .f = callStartitfied, size = "10k", proportionValue =  80, .progress = TRUE)

# temp export for presentation material 
write_csv(mlra80[[1]], "data/products/stratifiedSamplingResults/mlra80.csv")

# results at 90% agreement  -----------------------------------------------
mlra90 <- purrr::map(.x = featureAreas[1], .f = callStartitfied, size = "10k", proportionValue =  90)
# temp export for presentation material 
write_csv(mlra90[[1]], "data/products/stratifiedSamplingResults/mlra90.csv")



