pacman::p_load(terra,tigris, sf)

# read in template file 
r1 <- terra::rast("data/raw/nlcd/Annual_NLCD_LndCov_2020_CU_C1V1.tif") 

# pull state data 
ne <- tigris::states() |>
  dplyr::filter(NAME == "Nebraska")|>
  terra::vect() |>
  terra::project( crs(r1))

# crop files 
files <- list.files(
  path = "data/raw/nlcd",
  pattern = ".tif",
  full.names = TRUE
)
for(i in seq_along(files)){
  r1 <- terra::rast(files[i])
  name <- names(r1)
  export1 <- paste0("data/derived/nlcdData/", name,"_AEA.tif")
  export2 <- paste0("data/derived/nlcdData/", name,"_wgs84.tif")
  # crop and mask 
  r2 <- r1 |>
    terra::crop(ne) |>
    terra::mask(ne)
  # export 1 
  if(!file.exists(export1)){
    writeRaster(x = r2, filename = export1)
  }
  # reproject and export 
  if(!file.exists(export2)){
    r3 <- terra::project(r2, y = "EPSG:4326", method = "near")
    writeRaster(x = r3, filename = export2) 
  }
  try(rm(r1,r2,r3))
}


# process to AOI 
