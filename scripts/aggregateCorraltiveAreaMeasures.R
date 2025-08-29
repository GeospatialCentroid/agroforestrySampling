pacman::p_load(dplyr, readr, terra)

# work with forest layer for now 
path <- "data/products/nlcdMask"
f1 <- list.files(path, full.names = TRUE)

# forest 2016
t1 <- terra::vect(f1[5])

# get MLRA feature 
s1 <- terra::vect("data/derived/spatialFiles/CONUS_MLRA_52_dissolved.gpkg")

#for each feature in the MLRA, crop the forest layer and get a area 

data <- data.frame(
  MLRA_ID = s1$MLRA_ID,
  mlraArea = NA,
  forestArea = NA
)

for(i in 1:nrow(s1)){
  # select area 
  feat <- s1[i, ]
  # area of feature 
  totalArea <- terra::expanse(feat, unit = "km")
  # crop 
  t2 <- terra::crop(t1, feat)
  # area  
  forestArea <- terra::expanse(t2, unit = "km")
  # assign values 
  data[i,2:3] <-  c(totalArea, forestArea)
}
# temp export for examples
write_csv(data, file = "temp/forestArea2016.csv")
