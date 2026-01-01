# R/fct_processing.R

#' Add Z-score metrics and grouping
addZscoreMetrics <- function(df, target_cols) {
  for (col in target_cols) {
    if (!col %in% names(df)) {
      warning(paste("Column", col, "not found. Skipping."))
      next
    }
    
    mu <- mean(df[[col]], na.rm = TRUE)
    sigma <- sd(df[[col]], na.rm = TRUE)
    
    z_col_name <- paste0(col, "_Zscore")
    df[[z_col_name]] <- abs((df[[col]] - mu) / sigma)
    
    group_col_name <- paste0(col, "_group")
    df[[group_col_name]] <- dplyr::case_when(
      df[[z_col_name]] <= 1 ~ "0",
      df[[z_col_name]] > 1 & df[[z_col_name]] <= 2 ~ "1",
      df[[z_col_name]] > 2 ~ "2",
      TRUE ~ NA_character_
    )
  }
  return(df)
}

#' Helper to sum NLCD columns
sum_cols <- function(df, ...) {
  rowSums(dplyr::pick(..., data = df), na.rm = TRUE)
}

#' Process roads for a specific county list
processRoads <- function(countyIDs, roadFiles, mlra) {
  # Select road file matching county ID pattern
  # Note: Assumes roadFiles contains the full path strings
  r1 <- roadFiles[grepl(pattern = paste0("_", countyIDs, "_"), x = roadFiles)]
  
  if (length(r1) > 0) {
    r2 <- terra::vect(r1) |>
      terra::project(mlra) |>
      terra::intersect(mlra)
    return(r2)
  } else {
    return(NULL)
  }
}

#' Calculate road length per grid
processRoadLength <- function(gridID, grids1k, roads) {
  g1 <- grids1k[grids1k$id == gridID, ]
  
  # Intersect roads with specific grid cell
  r2 <- roads |>
    terra::intersect(g1) |>
    terra::perim()
  
  data.frame(
    gridID = g1$id,
    roadLengthKM = sum(r2, na.rm = TRUE) / 1000
  )
}

#' Process Riparian Area (Robust Version)
processRipArea <- function(gridID, grids1k, riparianData) {
  
  # 1. Subset the specific grid cell
  g1 <- grids1k[grids1k$id == gridID, ]
  
  # Calculate Total Area immediately (we always know the cell size)
  totArea <- terra::expanse(g1, unit = "km")
  
  # 2. Attempt Crop and Mask safely
  # We use tryCatch to handle cases where the grid cell is outside the raster
  r2 <- tryCatch({
    terra::crop(riparianData, g1, snap = "near") |> 
      terra::mask(g1)
  }, error = function(e) {
    return(NULL) 
  })
  
  # 3. Handle No-Data / No-Overlap Case
  if (is.null(r2)) {
    return(data.frame(
      gridID = g1$id,
      riparianArea = 0,
      totalArea = totArea,
      percentRiparian = 0
    ))
  }
  
  # 4. Calculate Riparian Area if overlap exists
  # terra::cellSize calculates the area of each pixel in km²
  pixel_areas <- terra::cellSize(x = r2, unit = "km")
  
  # Multiply pixel value (1 or 0) by area
  rip_surface <- r2 * pixel_areas
  rArea <- sum(terra::values(rip_surface), na.rm = TRUE)
  
  # 5. Return Results
  data.frame(
    gridID = g1$id,
    riparianArea = rArea,
    totalArea = totArea,
    percentRiparian = (rArea / totArea) * 100
  )
}

#' Process NLCD Area
processNLCDArea <- function(gridID, grids1k, nlcdData, year) {
  g1 <- grids1k[grids1k$id == gridID, ]
  
  r2 <- nlcdData |>
    terra::crop(g1) |>
    terra::mask(g1)
  
  # Calculate area by value (class)
  rastArea <- terra::expanse(x = r2, unit = "km", byValue = TRUE) 
  
  if(nrow(rastArea) == 0) return(NULL)
  
  rastArea |>
    dplyr::select(-layer) |>
    tidyr::pivot_wider(
      names_from = value,
      values_from = area,
      names_prefix = "class_"
    ) |>
    dplyr::mutate(
      total_rast_area = rowSums(dplyr::across(dplyr::starts_with("class_")), na.rm = TRUE),
      gridID = g1$id, 
      year = year
    ) |>
    dplyr::select(gridID, year, dplyr::everything())
}

#' Mosaic TOF rasters
gatherAllTOF <- function(mlra, g12, sel10m, year) {
  grids <- g12[mlra, ]
  ids <- grids$Unique_ID
  
  rasts <- terra::sprc()
  for (i in ids) {
    # Match pattern: ID_Year.tif
    p1 <- sel10m[grepl(pattern = paste0(i, "_", year, ".tif"), x = sel10m)]
    if(length(p1) > 0) rasts <- c(rasts, terra::rast(p1))
  }
  return(rasts)
}

#' Calculate TOF Area metrics with Error Handling
processTOF_areas <- function(gridID, grids1k, cotData) {
  g1 <- grids1k[grids1k$id == gridID, ]
  
  tryCatch({
    if (is.null(terra::intersect(terra::ext(g1), terra::ext(cotData)))) {
      stop("No overlap")
    }
    
    r2 <- cotData |>
      terra::crop(g1) |>
      terra::mask(g1)
    
    if (terra::ncell(r2) == 0) stop("Empty raster after crop")
    
    fullArea <- terra::cellSize(x = r2, unit = "km")
    ripArea <- r2 * fullArea
    
    totArea <- sum(terra::values(fullArea), na.rm = TRUE)
    rArea <- sum(terra::values(ripArea), na.rm = TRUE)
    
    data.frame(
      gridID = g1$id,
      tofArea = rArea,
      totalArea = totArea,
      percentTOF = (rArea / totArea) * 100
    )
  }, error = function(e) {
    data.frame(
      gridID = g1$id,
      tofArea = NA,
      totalArea = NA,
      percentTOF = NA
    )
  })
}