
pacman::p_load(sf, dplyr)

generateAOI <- function(id, type){
  if(type == "state"){
    aoi <- sf::st_read("data/derived/us/lower48.gpkg")
    aoi <- aoi[aoi$name %in% id, ]
  }
  if(type == "lrr"){
    aoi <- sf::st_read("data/derived/mlra/lower48LRR.gpkg")
    aoi <- aoi[aoi$LRRSYM %in% id, ]
  }
  if(type == "mlra"){
    aoi <- sf::st_read("data/derived/mlra/lower48MLRA.gpkg")
    aoi <- aoi[aoi$MLRA_ID %in% id, ]
  }
  # dissolve to a single object 
  aois <- sf::st_union(aoi)
  return(aois)
}
