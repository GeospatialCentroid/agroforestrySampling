pacman::p_load(dplyr, terra, sf, stringr)

source("functions/naipScrape.R")

# 100mile grid
g100 <- sf::st_read("data/derived/grids/grid100km_aea.gpkg")

# read in the 2 mile validation sets
v_files <- list.files(
  path = "data/raw/2mileValidationDatasets/SNIC-Comparison/",
  full.names = TRUE,
  pattern = ".tif"
)
# read in reference naip
f_files <- list.files(
  path = "data/raw/2mileValidationDatasets/",
  full.names = TRUE,
  pattern = ".tif"
)

# Build relationship table
df_lookup <- data.frame(
  year = c("2020", "2020", "2020", "2016", "2016"),
  mile2_id = c("17663", "10625", "24675", "30823", "6621"),
  km1_id = c(
    "1415-3-12-4-1",
    "1413-1-6-15-2",
    "1482-4-4-3-2",
    "1544-1-18-1-4",
    "1352-3-5-c-1"
  ),
  stringsAsFactors = FALSE
)

# Container for loop results
results_list <- list()
out_dir <- "data/products/testingSnicAgainst2mile"
# flipped features 1, 2,3,4, 5
# iterater over the grid Id
for (i in 1:nrow(df_lookup)) {
  id2 <- df_lookup$mile2_id[i]
  id1 <- df_lookup$km1_id[i]
  yr <- df_lookup$year[i]

  # select naip raster
  r1 <- terra::rast(f_files[grepl(pattern = id2, x = f_files)])
  # select the validation raster
  v1 <- terra::rast(v_files[grepl(pattern = id2, x = v_files)])
  # flip if needed
  v1 <- terra::flip(v1, direction = "vertical")
  # Assign spatial information
  crs(v1) <- crs(r1)
  ext(v1) <- ext(r1)

  # visual eval
  terra::plot(r1)
  terra::plot(v1, add = TRUE, alpha = 0.3, )

  # get AOI
  aoi <- getAOI(grid100 = g100, id = id1) |>
    sf::st_transform("epsg:4326") |>
    terra::vect()

  # crop to 1km area
  v2 <- terra::crop(v1, aoi)

  # Convention: validation, year, 2mile id, 1km id, .tif
  v2_filename <- paste0("validation_", yr, "_", id2, "_", id1, ".tif")
  v2_path <- file.path(out_dir, v2_filename)
  terra::writeRaster(v2, v2_path, overwrite = TRUE)

  # classify: White (TP) or Green (FN) = Forest (1), else 0
  # Vectorized math is faster and safer than app() for this logic
  tp <- (v2[[1]] == 255 & v2[[2]] == 255 & v2[[3]] == 255)
  fn <- (v2[[1]] == 0 & v2[[2]] == 255 & v2[[3]] == 0)
  val_binary <- (tp | fn) * 1

  # Get counts
  pixel_counts <- as.data.frame(freq(val_binary))

  # Extract values safely
  forest_val <- sum(pixel_counts$count[pixel_counts$value == 1], na.rm = TRUE)
  total_val <- sum(pixel_counts$count, na.rm = TRUE)
  pct_forest <- (forest_val / total_val) * 100

  # Append to results list
  results_list[[i]] <- data.frame(
    year = yr,
    mile2_id = id2,
    km1_id = id1,
    forest_pixels = forest_val,
    total_pixels = total_val,
    percent_forest = round(pct_forest, 4)
  )

  message(paste("Processed:", id1, "| Forest %:", round(pct_forest, 2)))
}

# Combine all iterations into one dataframe
final_df <- do.call(rbind, results_list)

# View and Export
print(final_df)
write.csv(
  final_df,
  "temp/m2_1km_forest_validation_summary.csv",
  row.names = FALSE
)
