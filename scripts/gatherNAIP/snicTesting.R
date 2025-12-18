# install.packages("snic")
pacman::p_load(snic, terra)

# 1. Read and Scale
r1 <- terra::rast("data/derived/naipExports/naip_2020_id_1349-1-a-10-3_wgs84.tif")
r1 <- r1 / 255


# 3. Generate Seeds
seeds <- snic_grid(r1, type = "hexagonal", spacing = 20L)

# 4. Run SNIC
segmentation <- snic(r1, seeds = seeds, compactness = 0.5)

# get raster and plot 
seg_raster <- snic::snic_get_seg(segmentation)
snic_plot(r1, r = 1, g = 2, b = 3, seg = segmentation)
# 5. Extract and Check
means <- snic_get_means(segmentation)
centroid <- snic_get_centroids(segmentation)
# Verify results
if(!is.null(means)) {
  print(head(means))
} else {
  print("Means is still NULL. Check r1 for unexpected values.")
}









pacman::p_load(snic, terra)

# Load and strictly clean
r1 <- terra::rast("data/derived/naipExports/naip_2020_id_1349-1-a-10-3_wgs84.tif")
r1 <- r1 / 255
r1 <- terra::subst(r1, NA, 0)

# Force into an array to bypass terra-specific pointer issues
r_arr <- terra::as.array(r1)

# Generate seeds using the original raster for geometry
# 1. Create the base high-density seed set (20L)
seeds_10 <- snic_grid(r1, type = "hexagonal", spacing = 10L)
seeds_20 <- seeds_10[seq(1, nrow(seeds_10), by = 2), ]
seeds_40 <- seeds_10[seq(1, nrow(seeds_10), by = 4), ]
seeds_100 <- seeds_10[seq(1, nrow(seeds_10), by = 10), ]
seeds_200 <- seeds_10[seq(1, nrow(seeds_10), by = 20), ]

seeds <- list(seeds_10,seeds_20,seeds_40,seeds_100, seeds_200)
labels <- c("s10","s20","s40","s100", "s200")
for(i in seq_along(labels)){
  seed <- seeds[[i]]
  label <- labels[i]
  # produce the segementation 
  # Run SNIC on the array
  segmentation <- snic(r1, seeds = seed, compactness = 0.2)
  seg_rast <- snic::snic_get_seg(x = segmentation)
  # plot results 
  snic::snic_plot(x = r1, r = 1,g = 2,b = 3, seg = segmentation )
  # export the feature as a vector 
  seg_poly <- terra::as.polygons(seg_rast, dissolve = TRUE)
  # export 
  export <- paste0("temp/segTest/seg_",label,".gpkg")
  if(!file.exists(export)){
    terra::writeVector(x = seg_poly, filename = export)
  }
}

# plot results 
snic::snic_plot(x = r1, r = 1,g = 2,b = 3, seg = segmentation )
# snic_animation(x = r1, seeds = seeds, file_path = "temp/test.gif")
# exporting 



