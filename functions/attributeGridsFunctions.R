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
  # reassign values to set everything equal to 1
  r1[!is.na(r1)] <- 1
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
  nlcd_cropped <- terra::crop(nlcd, aoi_proj, mask = TRUE) |>
    terra::project("epsg:4326", method = "near")

  return(nlcd_cropped)
}
calculateRastByMLRA <- function(rast, mlra_vect) {
  # Get number of MLRAs
  n_mlra <- nrow(mlra_vect)

  # Process each MLRA one at a time with progress
  result <- purrr::map_dfr(1:n_mlra, function(i) {
    message("Processing MLRA ", i, " of ", n_mlra)

    # Extract single MLRA polygon
    single_mlra <- mlra_vect[i, ]
    mlraArea <- terra::expanse(x = single_mlra, unit = "km")
    # crop to the extent of mlra reproject then process
    rast_2 <- rast |>
      terra::crop(single_mlra) |>
      terra::mask(single_mlra)
    # calculate the non NA area of raster
    rastArea <- terra::expanse(x = rast_2, unit = "km", byValue = TRUE) |>
      dplyr::select(-layer) |>
      tidyr::pivot_wider(
        names_from = value,
        values_from = area,
        names_prefix = "class_"
      ) |>
      dplyr::mutate(
        total_rast_area = rowSums(
          dplyr::across(dplyr::starts_with("class_")),
          na.rm = TRUE
        )
      )

    # Get attributes for this MLRA
    mlra_df <- as.data.frame(single_mlra) |>
      dplyr::select(MLRA_ID, MLRARSYM, MLRA_NAME) |>
      dplyr::bind_cols(rastArea) |>
      dplyr::mutate(
        totalArea = mlraArea,
        rast_percent = (total_rast_area / mlraArea) * 100
      )
    # Force garbage collection to free memory
    gc()
    # remove objects
    rm(rast_2, single_mlra)

    return(mlra_df)
  })

  return(result)
}


# calculate road length by MLRA
calculateRoadsByMLRA <- function(roads_vect, mlra_vect) {
  # Get number of MLRAs
  n_mlra <- nrow(mlra_vect)

  # Process each MLRA one at a time with progress
  result <- purrr::map_dfr(1:n_mlra, function(i) {
    message("Processing MLRA ", i, " of ", n_mlra)

    # Extract single MLRA polygon
    single_mlra <- mlra_vect[i, ]
    # Get MLRA area in km²
    mlraArea <- terra::expanse(single_mlra, unit = "km")

    # Crop and mask roads to MLRA
    roads_in_mlra <- terra::vect(roads_vect) |>
      terra::crop(single_mlra) |>
      terra::mask(single_mlra)

    # Calculate total road length
    if (nrow(roads_in_mlra) > 0) {
      # Get length in meters, convert to km
      road_length_m <- sum(terra::perim(roads_in_mlra))
      road_length_km <- road_length_m / 1000
      road_count <- nrow(roads_in_mlra)
    } else {
      road_length_km <- 0
      road_count <- 0
    }

    # Get attributes for this MLRA
    mlra_df <- as.data.frame(single_mlra) |>
      dplyr::select(MLRA_ID, MLRARSYM, MLRA_NAME) |>
      dplyr::mutate(
        total_area_km2 = mlraArea,
        road_length_km = road_length_km,
        road_count = road_count,
        road_density_km_per_km2 = road_length_km / mlraArea
      )

    # Force garbage collection to free memory
    gc()
    # remove objects
    rm(roads_in_mlra, single_mlra)

    return(mlra_df)
  })

  return(result)
}


# TOF calculations  ------------------------------------------------------
# cot_paths <- tar_read(cotPaths)
# grid_12 <- tar_read(grid12)
# mlra_vect <- tar_read(mlraAtt)

# helper funtion for processing
# some cases where the extent the raster objects dont match so crop and mask indiually then combine
rastCropMask <- function(rast, aoi) {
  r1 <- terra::rast(rast) |>
    terra::crop(aoi) |>
    terra::mask(aoi)
}


process_single_grid <- function(grid_id, selected_grids, cot_paths) {
  # gather the cropped area
  aoi <- selected_grids[selected_grids$Unique_ID == grid_id, ]

  # gather raster objects
  paths <- cot_paths[grepl(pattern = paste0(grid_id, "_"), x = cot_paths)]
  # prefroentially select
  cleaned_paths <- tibble(path = paths) |>
    dplyr::mutate(
      # Extract year specifically following 'models'
      year = stringr::str_extract(path, "(?<=models)\\d{4}"), #pull year from the /modelsYEAR/ section of the path
      is_harmonized = stringr::str_detect(path, "harmonized")
    ) |>
    dplyr::group_by(year) |>
    # If any path in this year is harmonized, keep only harmonized.
    # Otherwise, keep whatever is there.
    dplyr::filter(if (any(is_harmonized)) is_harmonized else TRUE) |>
    dplyr::ungroup() |>
    dplyr::pull(path)
  # process into single rast object
  originalRasts <- cleaned_paths |>
    purrr::map(rastCropMask, aoi = aoi) |>
    terra::rast()

  names(originalRasts) <- paste0(grid_id, c("_2010", "_2016", "_2020"))
  # get the area of the cells
  rastArea <- terra::expanse(
    x = originalRasts,
    unit = "km",
    byValue = TRUE
  ) |>
    tidyr::pivot_wider(
      names_from = value,
      values_from = area,
      names_prefix = "class_"
    ) %>% # mixed pipe to use '.'
    dplyr::mutate(
      class_0 = if ("class_0" %in% names(.)) class_0 else 0,
      class_1 = if ("class_1" %in% names(.)) class_1 else 0
    ) |>
    dplyr::mutate(
      total_rast_area = rowSums(
        dplyr::across(dplyr::starts_with("class_")),
        na.rm = TRUE
      ),
      layer = names(originalRasts),
      TOF_Percent = (class_1 / total_rast_area) * 100
    ) |>
    tidyr::separate(
      col = layer,
      into = c("gridID", "year"),
      sep = "_",
      convert = TRUE
    )

  return(rastArea)
}


process_single_mlra <- function(mlra_index, mlra_vect, grid_12, cot_paths) {
  n_mlra <- nrow(mlra_vect)
  message("Processing MLRA ", mlra_index, " of ", n_mlra)

  single_mlra <- mlra_vect[mlra_index, ]

  # filter the grid 12 object to the mlra
  selected_grids <- terra::vect(grid_12) |>
    terra::crop(single_mlra) |>
    terra::intersect(single_mlra)

  ids <- selected_grids$Unique_ID
  n_ids <- length(ids)

  export1 <- paste0(
    "data/derived/mlraSummaries/tofArea_mlra",
    single_mlra$MLRA_ID,
    ".csv"
  )

  if (!file.exists(export1)) {
    result2 <- purrr::map_dfr(ids, function(id) {
      # Optional: add progress message here if desired
      # message("Processing grid ", id)
      res <- process_single_grid(id, selected_grids, cot_paths)
      gc() # Free memory after each grid
      return(res)
    })

    dir.create(dirname(export1), showWarnings = FALSE, recursive = TRUE)
    readr::write_csv(result2, export1)
  } else {
    message("Reading cached data for MLRA ", single_mlra$MLRA_ID)
    result2 <- readr::read_csv(export1, show_col_types = FALSE)
  }

  result_sum <- result2 |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      total_class_1 = sum(class_1, na.rm = TRUE),
      total_area = sum(total_rast_area, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      TOF_Percent = (total_class_1 / total_area) * 100,
      MLRA_ID = single_mlra$MLRA_ID,
      MLRA_NAME = single_mlra$MLRA_NAME
    )

  return(result_sum)
}


calculateTotalTOF <- function(cot_paths, grid_12, mlra_vect) {
  n_mlra <- nrow(mlra_vect)

  result <- purrr::map_dfr(
    1:n_mlra,
    process_single_mlra,
    mlra_vect = mlra_vect,
    grid_12 = grid_12,
    cot_paths = cot_paths
  )

  return(result)
}


# process data to the smaller grids within an mlra
processSubGridAreas <- function(grid_feature, raster_layer, current_mlra_id) {
  grid_feature <- grid_feature[
    grid_feature$MLRA_ID == current_mlra_id,
  ]
  # get the area of features
  grid_feature <- terra::vect(grid_feature)
  grid_feature$gridArea <- terra::expanse(grid_feature, unit = "km")
  # temp id for join data back too
  grid_feature$ID <- 1:nrow(grid_feature)

  # spatial filter the raster data
  rastVals <- raster_layer |>
    terra::crop(grid_feature) |>
    terra::mask(grid_feature)
  rm(raster_layer)
  # convert the rast to area measures
  rast_cell_areas <- terra::cellSize(rastVals, unit = "km") |>
    terra::mask(rastVals)
  # extract the areas
  extractAreas <- terra::extract(
    rast_cell_areas,
    grid_feature
  )
  rm(rast_cell_areas)
  # extract values from raster object
  extractVals <- terra::extract(
    rastVals,
    grid_feature
  ) |>
    dplyr::mutate(area = extractAreas$area)
  rm(rastVals)
  names(extractVals) <- c("ID", "class", "area")
  # summarize by id

  # determine the
  class_counts <- extractVals |>
    dplyr::group_by(ID, class) |>
    dplyr::summarise(
      totalCount = n(),
      valArea = sum(area, na.rm = TRUE),
      .groups = "drop"
    )
  rm(extractVals)
  gc()

  # add the area of the id
  wide_df <- class_counts |>
    pivot_wider(
      id_cols = ID,
      names_from = class,
      values_from = c(totalCount, valArea),
      values_fill = 0 # Optional: replaces NA with 0
    )
  # join this back to the mlra File and export
  export <- grid_feature |>
    as.data.frame() |>
    dplyr::left_join(wide_df, by = "ID") %>%
    dplyr::mutate(across(
      .cols = starts_with("valArea_"),
      .fns = ~ (.x / gridArea) * 100,
      .names = gsub("valArea", "percentArea", "{.col}")
    )) |>
    rowwise() |>
    mutate(
      totalPercentArea = sum(c_across(starts_with("valArea_")))
    ) |>
    ungroup()
  return(export)
}
