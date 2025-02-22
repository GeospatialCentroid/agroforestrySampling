library(terra, tigris)


# list all features 
areas <- list.files(path = "data/raw/spatialAreaFiles",
                    pattern = ".gpkg",
                    full.names = TRUE,
                    recursive = TRUE)
# states 
states<- tigris::states() 
ne <- terra::vect(states[states$NAME=="Nebraska",]) |>
  terra::project("EPSG:4326")

for(i in areas){
  name <- basename(i)
  # read in and crop 
  r1 <- terra::vect(i) |>
    terra::crop(ne)
  # export 
  terra::writeVector(r1,
                     filename = paste0("data/derived/spatialFiles/",name ),
                     overwrite = TRUE)
}
