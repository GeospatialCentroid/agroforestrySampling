
  
pacman::p_load("terra", "dplyr", "readr", "sf", "tictoc", "tmap")


# goal 
## want to generate a systematic sample of a AOI 
## these are iterative process that will continue until 
## 90% of the draws at a specific number result in value within +/- 10% of the know value 


# levels of analysis 
## geographic stratification  (ecoregions, koppen climate, lrr, mlra)

# default inputs  ---------------------------------------------------------
grids <- getGrids(year = 2010)
# geographic stratification -----------------------------------------------
# based on the type gather 
# options are eco, MLRA, LRR, Koppen

for(areaType in c("eco", "MLRA", "LRR", "Koppen")){
  
  # pull area specific info 
  geoPaths <- getGeographicAreas(areaType = areaType)
  columnID <- geoPaths$columnID
  
  vect <- geoPaths$vect |> terra::vect() |> terra::aggregate(by = columnID)
  files <- geoPaths$files
  allNames <- unique(vect[,columnID]) |>
    as.data.frame() |> 
    pull()
  ## sub units within each stratification 
  ### this provides all the require information for the sampling method 
  subUnits <- purrr::map(.x = allNames, 
                         .f = subUnitSelection,
                         columnID = columnID,
                         vect = vect,
                         files = files)
  
  
  
  
  # Run Random Sampling  ----------------------------------------------------
  ## for each year 
  ## set threshold range 
  ## sample N values and average results 
  ## repeat 20 times 
  ## test to see if 18 of the twenty samples fall within the range, then stop 
  
  plan(strategy = "multicore", workers = 12)
  rand2 <- furrr::future_map(.x = subUnits, .f = runStratifiedSample) |>
    dplyr::bind_rows()
  write.csv(x = rand2,file =  paste0("data/derived/areaCounts/stratified_",areaType,".csv"))
}








