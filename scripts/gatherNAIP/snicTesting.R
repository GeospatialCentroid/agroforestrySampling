

pacman::p_load(snic, terra)

# Load and strictly clean
r1 <- terra::rast("data/derived/naipExports/naip_2020_id_19763_wgs84.tif")
r1 <- r1 / 255
r1 <- terra::subst(r1, NA, 0)

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
  export <- paste0("temp/segTest/seg_19763_",label,".gpkg")
  if(!file.exists(export)){
    terra::writeVector(x = seg_poly, filename = export)
  }
}

# plot results 
snic::snic_plot(x = r1, r = 1,g = 2,b = 3, seg = segmentation )
# snic_animation(x = r1, seeds = seeds, file_path = "temp/test.gif")
# exporting 



