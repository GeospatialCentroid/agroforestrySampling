###
# over all goal; a standalone workflow that can be used to download NAIP imagery from mircosoft planetary computer for a set AOI

# libraries
pacman::p_load(rstac, sf, terra, dplyr, tmap, rlang, httr, tictoc, purrr, furrr)
tmap_mode("view")

source("functions/naipScrape.R")

# required inputs  --------------------------------------------------------
grid100 <- sf::st_read("data/derived/grids/grid100km_aea.gpkg")
# test the 2mile grid
# Validated_X12-624_26060_2020_dealiased
twoMile <- sf::st_read("data/products/modelGrids/two_sq_grid.gpkg")


# regenerating 2 mile grids  ---------------------------------------------
# establish table of naip dates and grid elements
n10 <- c(1200, 2403, 16513, 27785, 26060)
n16 <- c(12560, 17395, 30590)
n20 <- c(
  1203,
  2572,
  5238,
  5551,
  8690,
  9472,
  12000,
  12632,
  13638,
  24161,
  24675,
  26060
)
# store as df
df <- data.frame(
  year = c(rep(2010, 5), rep(2016, 3), rep(2020, 12)),
  gridID = c(n10, n16, n20)
)

process_naip_2mile <- function(
  year,
  gridID,
  twoMilePath = "data/products/modelGrids/two_sq_grid.gpkg"
) {
  # Create a specific temp sub-folder
  temp_dir <- file.path("temp", paste0(year, "_", gridID))
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }

  # set aoi
  ## readin in within function to help with paralization
  twoMile <- sf::st_read(twoMilePath)
  aoi <- twoMile[twoMile$FID_two_grid == gridID, ] |>
    dplyr::select(id = FID_two_grid)

  # 3. Check NAIP availability
  naipYears <- getNAIPYear(aoi = aoi)

  if (year %in% naipYears) {
    message(paste("Processing Grid:", gridID, "Year:", year))
    out_path <- paste0(
      "data/derived/naipExports/naip_",
      year,
      "_id_",
      gridID,
      "_wgs84.tif"
    )
    if (!file.exists(out_path)) {
      # 4. Download
      downloadNAIP(aoi = aoi, year = year, exportFolder = temp_dir)
      # grab files
      files <- list.files(
        temp_dir,
        pattern = paste0(year, "_id_", gridID),
        full.names = TRUE
      )

      # 6. Standardize (Inner Loop)
      export_names <- paste0(
        stringr::str_remove(files, pattern = "\\.tif$"),
        "_wgs84.tif"
      )

      # Map the standardization function over the downloaded files
      purrr::walk2(.x = files, .y = export_names, .f = standardizeNAIP)

      # 7. Merge and Mask
      # Reload the standardized files
      rasters <- terra::sprc(lapply(export_names, terra::rast))

      m <- terra::merge(rasters, method = "bilinear") |>
        terra::mask(aoi)

      # Ensure output directory exists
      dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

      terra::writeRaster(m, filename = out_path, overwrite = TRUE)
    }

    #
  } else {
    warning(paste("Year", year, "not found for Grid", gridID))
  }

  # 9. Cleanup
  unlink(temp_dir, recursive = TRUE)
}

# Run the process for each year and grid ID
# purrr::pwalk(
#   df,
#   .f = process_naip_2mile
# )

future::plan(multisession, workers = 4)

furrr::future_pwalk(
  .l = df,
  .f = process_naip_2mile,
  .options = furrr::furrr_options(seed = TRUE)
)

# workflow ----------------------------------------------------------------
# aoi <- getAOI(grid100 = grid100, point = point)
# qtm(aoi)

# # test for year
# getNAIPYear(aoi = sel2)

# # download naip
# downloadNAIP(aoi = sel2, year = 2020, exportFolder = "temp")

# r1 <- terra::rast("temp/naip_2021_id_1998-3-12-4-4.tif")

# #standard
# standardizeNAIP(
#   importPath = "temp/naip_2021_id_1998-3-12-4-4.tif",
#   exportPath = "naip_2021_id_1998-3-12-4-4_wgs84.tif"
# )

# # download naip for the specific aoi for a specific year
# year <- "2021"
# exportFolder <- "temp"

# # test what years are available at the aoi
# getNAIPYear(aoi = aoi)

# # download area
# downloadNAIP(aoi = aoi, year = "2021", exportFolder = "temp")

# importPath <- "temp/naip_2021_id_1344-4-12-f-4.tif"

# # extra for the two mile grid testing  ------------------------------------
# # testing for the two mile
# files <- list.files("temp", pattern = "2020_id__", full.names = TRUE)
# export <- paste0(stringr::str_remove(files, pattern = "\\.tif$"), "_wgs84.tif")
# # only around 10g ram allocation so will work well for furrr distribution
# tic()
# purrr::map2(.x = files, .y = export, .f = standardizeNAIP)
# toc()
# # run time on 4 images : 5.5 minutes

# rasters <- terra::sprc(lapply(export, terra::rast))

# # high memory allocation with merge 20gb
# tic()
# m <- terra::merge(rasters)
# toc()
# # 4 features to 1 in 67 seconds
