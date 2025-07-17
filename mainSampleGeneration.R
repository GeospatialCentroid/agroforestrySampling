#
pacman::p_load(dplyr, terra,sf,readr)

# updated methods for sampling 
pacman::p_load("terra", "dplyr", "readr", "sf", "tictoc", "tmap")

#source 
source("functions/samplingWorkflowFunctions.R")

# grid feature 
grid <- st_read("data/products/newModelAreas/results10kGrid.gpkg")


# summarize data by subset regions ------------------------------------------
regions <- list.files("data/derived/spatialFiles",
                      full.names = TRUE)
eco <- regions[grepl(pattern = "us_eco", regions)] |>st_read()
mlra <- regions[grepl(pattern = "MLRA", regions)] |>st_read()
lrr <- regions[grepl(pattern = "LRR", regions)] |>st_read()
koppen <- regions[grepl(pattern = "Koppen", regions)] |>st_read()





# levels of analysis 
## geographic stratification  (ecoregions, koppen climate, lrr, mlra)

# default inputs  ---------------------------------------------------------
# 10k grids 

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








