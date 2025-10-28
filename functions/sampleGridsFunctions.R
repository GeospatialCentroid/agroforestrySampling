# generate the a grid object  --------------------------------------------------
## currently duplicated from the preprocessingFunction.R 
buildGrids <- function(extent_object, cell_size){
  # transform to equal area 
  ea <- sf::st_transform(extent_object, 5070)
  # generate grid 
  grid <- sf::st_make_grid(
    x = ea, 
    cellsize = cell_size
  )
  if("id" %in% names(ea)){
    ids <- paste0(ea$id[1],"-",as.hexmode(1:length(grid)))
  }else{
    ids = as.hexmode(1:length(grid))
  }
  # generate ID 
  gridID <- sf::st_sf(
    id = ids,
    geomentry = grid)
  # export 
  return(gridID)
}

# generate subgrids objects 
buildSubGrids <- function(grids, cell_size, aoi ){

  # generate sub grids 
  subGrids <- grids |>
    dplyr::group_split(id)|>
    purrr::map(.f = buildGrids,
               cell_size = cell_size) |>
    dplyr::bind_rows()
  # apply the filter again -- 
  subGrids <- subGrids[aoi, ]
  
  return(subGrids)
}


# select 100km in the aoi 
select100km <- function(original_100km, aoi_feature){
  val <-original_100km |> 
    st_filter(aoi_feature)
  return(val)
} 


# process grids to MLRA  --------------------------------------------------



# process grids to lrr ----------------------------------------------------






