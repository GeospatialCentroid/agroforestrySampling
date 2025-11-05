### attribute information to each grid
### TOF 10,16,20
### - get the reference intersections for each 12 mile grid
### - use that to pull in possible imagery

gatherTOF_Files <- function() {
  files <- list.files(
    path = "/home/dune/trueNAS/work/Agroforestry/data",
    pattern = "_Masked.tif",
    full.names = TRUE,
    recursive = TRUE
  )
  return(files)
}

gatherRiparian <- function() {
  r1 <- terra::rast(
    "/home/dune/trueNAS/work/Agroforestry/data/products/riparian/nebraskaRiparian10.tif"
  )
  return(r1)
}

gatherRoads <- function(aoi, stateFIPS) {
  # Ensure AOI is in WGS84 (EPSG:4326) for tigris
  aoi_wgs84 <- sf::st_transform(aoi, 4326)
  # Get the bounding box
  bbox <- sf::st_bbox(aoi_wgs84)
  # Download roads for each state and combine
  roads_list <- lapply(stateFIPS, function(stateFIPS) {
    tigris::primary_secondary_roads(state = stateFIPS, year = 2020)
  })
  # Combine all roads
  all_roads <- dplyr::bind_rows(roads_list) |>
    sf::st_transform(crs = 4326)

  # Filter to roads that intersect the AOI
  roads_in_aoi <- all_roads[aoi_wgs84, , op = sf::st_intersects]

  return(roads_in_aoi)
}

gatherNLCD <- function(aoi, nlcd_path) {
  # Read the NLCD raster
  nlcd <- terra::rast(nlcd_path)
  # Transform AOI to match NLCD CRS
  aoi_proj <- sf::st_transform(aoi, terra::crs(nlcd))
  # Crop and mask NLCD to AOI
  nlcd_cropped <- terra::crop(nlcd, aoi_proj, mask = TRUE)

  return(nlcd_cropped)
}

calculateRiparianByMLRA <- function(riparian_rast, mlra_vect) {
  # Extract riparian values for each MLRA polygon
  riparian_by_mlra <- terra::extract(
    riparian_rast,
    mlra_vect,
    fun = sum,
    na.rm = TRUE,
    ID = TRUE
  )

  # # Calculate area using polygon geometries instead of pixel counting
  # This is more accurate in WGS84
  mlra_areas <- terra::expanse(mlra_wgs84, unit = "km")

  # Get the proportion of riparian pixels per MLRA
  total_pixels_per_mlra <- terra::extract(
    riparian_rast,
    mlra_vect,
    fun = length,
    na.rm = TRUE,
    ID = TRUE
  )

  # Convert SpatVector attributes to dataframe
  result <- as.data.frame(mlra_wgs84) |>
    dplyr::mutate(
      mlra_id = dplyr::row_number(),
      # Calculate riparian area as proportion of total MLRA area
      riparian_area_km2 = (riparian_by_mlra[[2]] / total_pixels_per_mlra[[2]]) *
        mlra_areas
    )
  return(result)
}
