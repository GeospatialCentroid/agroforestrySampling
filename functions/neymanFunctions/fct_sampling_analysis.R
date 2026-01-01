run_neyman_pipeline <- function(clean_df, mlra_id, year, strat_var, dir_res, dir_plot) {
  
  message("      Running Sampling Analysis...")
  
  # 1. Prepare Data
  df_sampling <- clean_df |>
    dplyr::select(gridID, percentTOF, areas = gridArea, strat_var = dplyr::all_of(strat_var)) |>
    dplyr::filter(is.finite(strat_var)) |>
    dplyr::mutate(
      group_kmeans = kmeans(strat_var, centers = 3)$cluster,
      group_equal  = cut(strat_var, 3, labels = FALSE),
      group_srs    = 1
    )
  
  # 2. Allocations
  TOTAL_N <- 100
  
  alloc_kmeans <- get_neyman_allocation(df_sampling, group_kmeans, percentTOF, TOTAL_N, splitType = "kmeans")
  alloc_equal  <- get_neyman_allocation(df_sampling, group_equal, percentTOF, TOTAL_N, splitType = "equal")
  alloc_srs    <- get_neyman_allocation(df_sampling, group_srs, percentTOF, TOTAL_N, splitType = "SRS")
  
  data_list <- list(
    kmeans = list(data = alloc_kmeans, name = "group_kmeans"),
    equal  = list(data = alloc_equal,  name = "group_equal"),
    srs    = list(data = alloc_srs,    name = "group_srs")
  )
  
  # 3. Simulation
  totalArea <- sum(df_sampling$areas, na.rm = TRUE)
  fullRatio <- weighted.mean(df_sampling$percentTOF, df_sampling$areas, na.rm = TRUE)
  tofMeasured <- (fullRatio / 100) * totalArea
  
  final_results <- purrr::map_dfr(
    data_list, 
    ~runSampling_neyman(.x, df_sampling, totalArea, 100, 
                        tofMeasured - (tofMeasured * 0.1), 
                        tofMeasured + (tofMeasured * 0.1), 
                        tofMeasured)
  )
  
  # --- NEW: DYNAMIC FILENAME WITH STRAT_VAR ---
  # Ensures output file reflects the variable used (e.g., _Forest.csv)
  out_name <- paste0("neymanResults_MLRA", mlra_id, "_", year, "_", strat_var, ".csv")
  
  if(!dir.exists(dir_res)) dir.create(dir_res, recursive = TRUE)
  write_csv(final_results, file.path(dir_res, out_name))
  # --------------------------------------------
  
  return(final_results)
}