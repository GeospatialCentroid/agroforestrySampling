process_static_mlra <- function(mlra_id, 
                                road_year = 2020, 
                                state_abbr = "NE",
                                path_mlra, 
                                path_grids, 
                                path_riparian, 
                                dir_vect, 
                                dir_tab,
                                counties) {
  
  message("--- Processing Static Layers for MLRA: ", mlra_id, " ---")
  
  # 1. Load Base Shapes
  # ---------------------------------------------------------
  mlra_vect <- terra::vect(path_mlra)
  m1 <- mlra_vect[mlra_vect$MLRA_ID == mlra_id, ]
  
  grids_vect <- terra::vect(path_grids)
  k1 <- grids_vect[grids_vect$MLRA_ID == mlra_id, ]
  
  
  # 2. Process Roads (Download -> Clip -> Save)
  # ---------------------------------------------------------
  # roadExport <- file.path(dir_vect, paste0("mlra", mlra_id, "_Roads.gpkg"))
  # 
  # if (!file.exists(roadExport)) {
  #   message("   Roads file not found. Downloading and processing...")
  #   options(timeout = 600) 
  #   options(tigris_refresh = TRUE)
  #   # A. Get Counties that overlap the MLRA
  #   # Spatial filter: Keep only counties touching the MLRA
  #   target_counties <- counties[m1, ] 
  #   county_codes <-  target_counties$COUNTYFP
  #   
  #   message("   Found ", length(county_codes), " overlapping counties.")
  #   
  #   # B. Helper function to download and clean a single county
  #   get_county_roads <- function(code, mlra_shape) {
  #     # explicit internal tryCatch for individual county failures
  #     tryCatch({
  #       options(tigris_refresh = TRUE)
  #       # 1. Download (force fresh download if previous failed)
  #       # We try-catch the download specifically to handle timeout/connection issues
  #       r_path <- tryCatch(
  #         tigris::roads(state = state_abbr, 
  #                       county = code),
  #         error = function(e) return(NULL)
  #       )
  #       
  #       if (is.null(r_path)) {
  #         warning("   Download failed for county: ", code)
  #         return(NULL)
  #       }
  #       
  #       # 2. Load into Terra
  #       # We explicitly check if the file can be read
  #       v <- tryCatch(terra::vect(r_path), error = function(e) NULL)
  #       
  #       if (is.null(v)) {
  #         warning("   Corrupt file for county: ", code)
  #         return(NULL)
  #       }
  #       
  #       # 3. Project and Crop
  #       v <- terra::project(v, terra::crs(mlra_shape))
  #       v_clipped <- terra::crop(v, mlra_shape)
  #       
  #       return(v_clipped)
  #       
  #     }, error = function(e) {
  #       warning("   Unexpected error for county: ", code, " - ", e$message)
  #       return(NULL)
  #     })
  #   }
  #   
  #   # C. Map over counties and combine
  #   all_roads_list <- purrr::map(
  #     .x = county_codes, 
  #     .f = get_county_roads, 
  #     mlra_shape = m1
  #   )
  #   
  #   # Bind into one object
  #   roads <- all_roads_list |> 
  #     purrr::discard(is.null) |> 
  #     terra::vect()
  #   
  #   # D. Save to disk
  #   terra::writeVector(x = roads, filename = roadExport, overwrite = TRUE)
  #   message("   Roads saved to: ", roadExport)
  #   
  # } else {
  #   message("   Loading existing roads from: ", roadExport)
  #   roads <- terra::vect(roadExport)
  # }
  # 
  # 
  # # 3. Calculate Road Statistics (Grid Intersection)
  # # ---------------------------------------------------------
  # # Requires your 'processRoadLength' function to be sourced globally
  # roadLengthExport <- file.path(dir_tab, paste0("roadLength_mlra", mlra_id, ".csv"))
  # 
  # if (!file.exists(roadLengthExport)) {
  #   message("   Calculating Road Lengths per grid cell...")
  #   road_stats <- purrr::map_dfr(k1$id, ~processRoadLength(.x, k1, roads))
  #   readr::write_csv(road_stats, roadLengthExport)
  # } else {
  #   road_stats <- readr::read_csv(roadLengthExport)
  # }
  
  
  # 4. Process Riparian Area
  # ---------------------------------------------------------
  ripAreaExport <- file.path(dir_tab, paste0("riparianArea_mlra", mlra_id, ".csv"))
  
  if (!file.exists(ripAreaExport)) {
    message("   Calculating Riparian Areas...")
    # Load and crop riparian raster only when needed to save RAM
    rip_rast <- terra::rast(path_riparian) |> terra::crop(m1)
    
    rip_stats <- purrr::map_dfr(k1$id, ~processRipArea(.x, k1, rip_rast))
    readr::write_csv(rip_stats, ripAreaExport)
  } else {
    rip_stats <- readr::read_csv(ripAreaExport)
  }
  
  
  # 5. Return List of Static Assets
  # ---------------------------------------------------------
  return(list(
    mlra_shape = m1,
    grid_shape = k1,
    # road_stats = road_stats,
    rip_stats = rip_stats
  ))
}
