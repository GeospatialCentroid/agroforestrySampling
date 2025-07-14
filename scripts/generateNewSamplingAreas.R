# method for producing new summary areas, then getting pixel counts on the specific models 

pacman::p_load(terra,sf,purrr, dplyr, readr, tigris, tidyr)


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
k10 <- meterToDegree(10000) # 10 km
k5 <- meterToDegree(5000)
k2 <- meterToDegree(2000)

# Use makeGrid() to create the grid of square polygons
gridVect <- sf::st_make_grid(neb, cellsize = k10)|>
  st_intersection(st_as_sf(neb))|>
  terra::vect()|>
  st_as_sf() |>
  dplyr::mutate(ID = dplyr::row_number())

# test intersection between 12 mile grids and new areas 
grid12 <- st_read("data/products/modelGrids/modelGrids_2010.gpkg")

# all COT files 
cot <- list.files(path = "~/trueNAS/work/Agroforestry/data/products/changeOverTime",
                  pattern = "_2.tif",
                  full.names = TRUE)

processSubGrids <- function(index,gridVect, grid12, cot){
  # select 12 mile grid area 
  g12 <- grid12[index,]
  # grid ID 
  gID <- g12$Unique_ID
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
  df <- df[,2:10]
  # add back to spatial features 
  subGrids <- subGrids |>
    bind_cols(df)
  st_write(subGrids, dsn = paste0("data/derived/subGridSelections/",gID,"_subGrids.gpkg"))
}

purrr::map(.x = 1:2, .f = processSubGrids,
           gridVect = gridVect,
           grid12 = grid12,
           cot = cot)
