pacman::p_load(dplyr, terra,tmap,readr)
tmap::tmap_mode("view")

# Build relationship table
## so something if off with the 12mile grid id
df_lookup <- data.frame(
  year = c("2020", "2020",  "2016", "2016"),
  mile2_id = c("17663", "10625",  "30823", "6621"),
  km1_id = c(
    "1415-3-12-4-1",
    "1413-1-6-15-2",
    "1544-1-18-1-4",
    "1352-3-5-c-1"
  ),
  stringsAsFactors = FALSE
)



## input data 
unet<- list.files(path = "~/trueNAS/work/agroforestrySampling/SNIC-Comparison/1km_test_grids", full.names = TRUE)
preRF16 <- list.files("~/trueNAS/work/Agroforestry/data/products/models2016/fullImages", 
                      full.names=TRUE) 
preRF20 <-  list.files("~/trueNAS/work/Agroforestry/data/products/models2020/fullImages", 
                       full.names=TRUE) 
rfAll <- c(preRF16, preRF20)

rfVal <- list.files("~/trueNAS/work/agroforestrySampling/SNIC-Comparison", full.names = TRUE)
unetPred <- unet[grepl(pattern = "_pred.tif", x = unet)]
unetVal <- unet[grepl(pattern = "_final.tif", x = unet)]

# naip <- unet[grepl(pattern = "_mosaic.tif", x = unet)]
naip <- list.files(path = "~/trueNAS/work/agroforestrySampling/data/ready_for_export", pattern = "naip_", full.names = TRUE, recursive = TRUE)
# 2 mile grid 
mile2 <- terra::vect("~/trueNAS/work/agroforestrySampling/data/derived/grids/two_sq_grid.gpkg")
mile12 <- terra::vect("~/trueNAS/work/agroforestrySampling/data/raw/grid12M/twelve_mi_grid_uid.gpkg")


# Container for loop results
results_list <- list()
out_dir <- "data/products/testingSnicAgainst2mile"
# flipped features 1, 2,3,4, 5
# iterater over the grid Id
for (i in 1:nrow(df_lookup)) {
  id2 <- df_lookup$mile2_id[i]
  id1 <- df_lookup$km1_id[i]
  yr <- df_lookup$year[i]
  
  # two milearea 
  m2 <- mile2[mile2$FID_two_grid == id2, ]
  # select the 12 mile grid that intersects 
  c1 <- terra::centroids(m2)  
  m12 <- mile12[c1]
  mile12_id <- m12$Unique_ID

  
  # select naip raster
  naip1 <- terra::rast(naip[grepl(pattern = id1, x = naip)]) 

  # select the validations raster
  rfVali <- terra::rast(rfVal[grepl(pattern = id2, x = rfVal)]) |>
    terra::flip(direction = "vertical")
  # Assign spatial information
  crs(rfVali) <- crs(naip1)
  ext(rfVali) <- ext(m2)
  rfV <- terra::crop(x = rfVali, naip1)
  # classify: White (TP) or Green (FN) = Forest (1), else 0
  # Vectorized math is faster and safer than app() for this logic
  tp <- (rfV[[1]] == 255 & rfV[[2]] == 255 & rfV[[3]] == 255)
  fn <- (rfV[[1]] == 0 & rfV[[2]] == 255 & rfV[[3]] == 0)
  val_binary <- (tp | fn) * 1
  
  # select the classified data 
  rfYear <- rfAll[grepl(pattern = paste0("models",yr), x = rfAll)]
  rfSel <- rfYear[grepl(pattern = paste0(mile12_id, "_f"), x = rfYear)]
  
  rfPred <- terra::rast(rfSel[!is.na(rfSel)]) 
  rfP <- terra::crop(rfPred, naip1)
  
  
  # unet validation 
  u1 <- terra::rast(unetVal[grepl(pattern = id1, x = unetVal)])
  # flip if needed
  unetVals <- terra::flip(u1, direction = "vertical")

  # Assign spatial information
  crs(unetVals) <- crs(naip1)
  ext(unetVals) <- ext(naip1)
  tp <- (unetVals[[1]] == 255 & unetVals[[2]] == 255 & unetVals[[3]] == 255)
  allVals <-  values(tp)
  percentUnet <- (sum(allVals)/ length(allVals)) *100
  # export here with CRS information 
  terra::writeRaster(unetVals, paste0("SNIC-Comparison/1km_test_grids/wgs84_",id1,".tif"), overwrite =TRUE )
  
  # unet Pred
  unetPr <- terra::rast(unetPred[grepl(pattern = id1, x = unetPred)]) |>
    terra::project("EPSG:4326")
  
  # generate a frequency of all features 
  getForestPercent <- function(rast){
    # Get counts
    pixel_counts <- as.data.frame(freq(rast))
    # Extract values safely
    forest_val <- sum(pixel_counts$count[pixel_counts$value == 1], na.rm = TRUE)
    total_val <- sum(pixel_counts$count, na.rm = TRUE)
    pct_forest <- (forest_val / total_val) * 100
    return(pct_forest)
  }

  

  # storage data 
  results_list[[i]] <- data.frame(
    year = yr, 
    mile2_id = id2,
    km1_id = id1,
    rfP = getForestPercent(rfP),
    rfV = getForestPercent(val_binary),
    unetP = getForestPercent(unetPr),
    unetV = percentUnet
  )
  
}

# bind and export 
df2 <- bind_rows(results_list)
write_csv(df2,"SNIC-Comparison/areaCalCompare_20260210.csv" )
