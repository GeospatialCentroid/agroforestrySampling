# method for producing new summary areas, then getting pixel counts on the specific models 

pacman::p_load(terra,sf,purrr, dplyr, readr, tigris, tidyr,furrr)


nebFile <- "data/raw/spatialAreaFiles/nebraska.gpkg"
if(!file.exists(nebFile)){
  # nebraska as template 
  neb <- tigris::states() |>
    dplyr::filter(NAME == "Nebraska") |>
    sf::st_transform(crs = 4326)
  # export 
  sf::st_write(obj = neb, dsn = nebFile)
}else{
  neb <- sf::st_read(nebFile)
}

# Grid objects at different areas 
## assumtion 1 degree == 111,111m 
meterToDegree <- function(meters){
  vals <- meters / 111111
  return(vals)
}

makeGrid <- function(area, cellsize){
  # convert meters to degrees 
  size <- meterToDegree(cellsize)
  # generate spatial feature
  gridVect <- sf::st_make_grid(area, cellsize = size)|>
    st_intersection(st_as_sf(area))|>
    terra::vect()|>
    st_as_sf() |>
    dplyr::mutate(ID = dplyr::row_number())
  return(gridVect)
}
grid10 <- makeGrid(area = neb, cellsize = 10000)|>
  sf::st_write("data/derived/spatialFiles/grid10k.gpkg",delete_dsn = TRUE)
grid25 <- makeGrid(area = neb, cellsize = 25000)|>
  sf::st_write("data/derived/spatialFiles/grid25k.gpkg",delete_dsn = TRUE)
grid5 <- makeGrid(area = neb, cellsize = 5000)  |>
  sf::st_write("data/derived/spatialFiles/grid5k.gpkg",delete_dsn = TRUE)
    

# test intersection between 12 mile grids and new areas 
grid12 <- st_read("data/products/modelGrids/modelGrids_2010.gpkg")

# all COT files 
cot <- list.files(path = "~/trueNAS/work/Agroforestry/data/products/changeOverTime",
                  pattern = "_2.tif",
                  full.names = TRUE)

processSubGrids <- function(index,size,gridVect, grid12, cot){
  # select 12 mile grid area 
  g12 <- grid12[index,]
  # grid ID 
  gID <- g12$Unique_ID
  # exprot 
  exportPath <- paste0("data/derived/subGridSelections/",size,"/",gID,"_subGrids.gpkg")
  if(!file.exists(exportPath)){
    # select new subgrids 
    subGrids <- sf::st_intersection(gridVect, g12)
    # select raster object 
    r1 <- cot[grepl(pattern = paste0(gID,"_change"),x = cot)] |>
      terra::rast()
    r1 <- r1$ChangeOverTime
    # extract values 
    vals <- terra::extract(x = r1, y = terra::vect(subGrids))
    # summarize 
    df <- vals |>
      dplyr::group_by(ID)|>
      dplyr::count(ChangeOverTime)|>
      pivot_wider(names_from = ChangeOverTime, values_from = n)
    # drop columns 
    df <- df[,-1]
    # add back to spatial features 
    subGrids <- subGrids |>
      bind_cols(df)
    st_write(subGrids, dsn = exportPath)
  }
  gc()
  return("complete")
}
# 
# purrr::map(.x = 1:773, .f = processSubGrids,
#            gridVect = gridVect,
#            size = "10k",
#            grid12 = grid12,
#            cot = cot)
errors <- c()
for(i in 1:773){
  print(i)
  val <- try(processSubGrids(index = i,
                  size = "10k",
                  gridVect = gridVect,
                  cot = cot,
                  grid12 = grid12))
  if(class(val)=="try-error"){
    errors <- c(errors, i)
  }
}

# future::plan("multicore", workers = 2)
# furrr::future_map(.x = 1:773, .f = processSubGrids,
#                              gridVect = gridVect,
#                              size = "10k",
#                              grid12 = grid12,
#                              cot = cot)

# t1 <- sf::st_read("data/derived/subGridSelections/10k/X12-427_subGrids.gpkg")
# t2 <- sf::st_read("data/derived/subGridSelections/10k/X12-57_subGrids.gpkg")
# t3 <- sf::st_read("data/derived/subGridSelections/10k/X12-15_subGrids.gpkg")


# summarize the results  --------------------------------------------------
readAndDF <- function(path){
  t1 <- st_read(path)|>
    st_drop_geometry()
  return(t1)
}

summarizeResample  <- function(folderPath){
  files <- list.files(path = folderPath,
                      pattern = ".gpkg",
                      full.names = TRUE)  
  df <- purrr::map(.x = files,.f = readAndDF) |>
    bind_rows()

  return(df)
}


# 10k grid 
folderPath <- "data/derived/subGridSelections/10k"
exportPath <- "data/derived/gridAreaSummaries/grids10k.csv"
if(!file.exists(exportPath)){
  results <- summarizeResample(folderPath)
  # export 
  write_csv(results, file = exportPath)
}




