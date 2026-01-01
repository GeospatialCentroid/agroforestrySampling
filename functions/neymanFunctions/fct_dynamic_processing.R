process_dynamic_year <- function(mlra_id, year, static_data, path_g12, path_tof_dir, dir_rast, dir_tab) {
  
  message("   Processing Year: ", year, " for MLRA: ", mlra_id)
  
  k1 <- static_data$grid_shape
  m1 <- static_data$mlra_shape
  
  # --- A. NLCD Processing ---
  nlcd_name <- paste0("nlcd_", year)
  nlcd_obj  <- tryCatch(tar_read_raw(nlcd_name), error = function(e) NULL)
  
  if (is.null(nlcd_obj)) stop(paste("Target", nlcd_name, "not found!"))
  
  nlcd_crop <- terra::crop(nlcd_obj, m1)
  
  nlcdAreaExport <- file.path(dir_tab, paste0("nlcdArea_mlra", mlra_id, "_", year, ".csv"))
  if (!file.exists(nlcdAreaExport)) {
    nlcd_stats <- purrr::map_dfr(k1$id, ~processNLCDArea(.x, k1, nlcd_crop, year))
    readr::write_csv(nlcd_stats, nlcdAreaExport)
  } else {
    nlcd_stats <- readr::read_csv(nlcdAreaExport, show_col_types = FALSE)
  }
  
  # --- B. TOF Processing ---
  tof_export <- file.path(dir_rast, paste0("mlra", mlra_id, "_TOF_", year, ".tif"))
  if (!file.exists(tof_export)) {
    g12 <- terra::vect(path_g12)
    tof10m <- list.files(path_tof_dir, full.names = TRUE, recursive = TRUE)
    rasters <- gatherAllTOF(mlra = m1, g12 = g12, sel10m = tof10m, year = year) |>
      terra::mosaic(fun = "max")
    terra::writeRaster(rasters, tof_export, overwrite=TRUE)
  } else {
    rasters <- terra::rast(tof_export)
  }
  
  tofAreaExport <- file.path(dir_tab, paste0("tofArea_mlra", mlra_id, "_", year, ".csv"))
  if (!file.exists(tofAreaExport)) {
    tof_stats <- purrr::map_dfr(k1$id, ~processTOF_areas(.x, k1, rasters))
    readr::write_csv(tof_stats, tofAreaExport)
  } else {
    tof_stats <- readr::read_csv(tofAreaExport, show_col_types = FALSE)
  }
  
  # --- C. Feature Engineering ---
  sum_cols <- function(df, pattern) {
    cols <- dplyr::select(df, starts_with(pattern))
    if (ncol(cols) == 0) return(rep(0, nrow(df)))
    rowSums(cols, na.rm = TRUE)
  }
  
  nlcd_clean <- nlcd_stats |>
    dplyr::mutate(
      Water      = sum_cols(dplyr::pick(everything()), "class_1"),
      Developed  = sum_cols(dplyr::pick(everything()), "class_2"),
      Barren     = sum_cols(dplyr::pick(everything()), "class_3"),
      Forest     = sum_cols(dplyr::pick(everything()), "class_4"),
      Shrubland  = sum_cols(dplyr::pick(everything()), "class_5"),
      Herbaceous = sum_cols(dplyr::pick(everything()), "class_7"),
      Planted    = sum_cols(dplyr::pick(everything()), "class_8"),
      Wetlands   = sum_cols(dplyr::pick(everything()), "class_9")
    ) |>
    dplyr::select(gridID, Water, Developed, Barren, Forest, Shrubland, Herbaceous, Planted, Wetlands)
  
  full_df <- tof_stats |>
    dplyr::rename(gridArea = totalArea) |>
    dplyr::left_join(static_data$rip_stats, by = "gridID") |>
    dplyr::left_join(nlcd_clean, by = "gridID") |>
    dplyr::mutate(year = year, mlra = mlra_id)
  
  # --- NEW: EXPORT CLEAN MERGED DATA ---
  clean_export_path <- file.path(dir_tab, paste0("cleanData_mlra", mlra_id, "_", year, ".csv"))
  readr::write_csv(full_df, clean_export_path)
  # -------------------------------------
  
  return(full_df)
}