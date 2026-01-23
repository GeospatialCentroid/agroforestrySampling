run_neyman_pipeline <- function(clean_df, mlra_id, year, strat_var, dir_res, dir_plot) {
  
  message("      Running Sampling Analysis...")
  
  # --- 1. Prepare Data & Create Groups ---
  # We separate the stratification variable selection to make the code cleaner
  df_sampling <- clean_df |>
    dplyr::select(gridID, percentTOF, areas = gridArea, strat_var = dplyr::all_of(strat_var)) |>
    dplyr::filter(is.finite(strat_var)) |>
    dplyr::mutate(
      
      # A. Equal Interval (Existing)
      group_equal = cut(strat_var, 3, labels = FALSE),
      
      # B. K-Means (Existing) - With safety check for zero variance
      group_kmeans = {
        n_distinct_vals <- n_distinct(strat_var)
        k_centers <- ifelse(n_distinct_vals >= 3, 3, n_distinct_vals)
        if(var(strat_var) == 0) rep(1, n()) else kmeans(strat_var, centers = k_centers)$cluster
      },
      
      # C. Standard Quantile (NEW)
      # We use unique() in breaks to handle cases where multiple quantiles are 0
      group_quantile = cut(
        strat_var, 
        breaks = unique(quantile(strat_var, probs = seq(0, 1, length.out = 4), na.rm = TRUE)),
        include.lowest = TRUE, 
        labels = FALSE
      ),
      
      # D. Zero-Inflated Quantile (NEW)
      # Group 1: Exactly Zero
      # Group 2: Below Median of the non-zeros
      # Group 3: Above Median of the non-zeros
      group_customQuantile = dplyr::case_when(
        strat_var == 0 ~ 1,
        strat_var <= median(strat_var[strat_var > 0], na.rm = TRUE) ~ 2,
        TRUE ~ 3
      ),
      
      # E. SRS Baseline (Existing)
      group_srs = 1
    )
  
  
  # --- 2. Allocations ---
  TOTAL_N <- 100
  
  alloc_kmeans   <- get_neyman_allocation(df_sampling, group_kmeans, percentTOF, TOTAL_N, splitType = "kmeans")
  alloc_equal    <- get_neyman_allocation(df_sampling, group_equal, percentTOF, TOTAL_N, splitType = "equal")
  alloc_quantile <- get_neyman_allocation(df_sampling, group_quantile, percentTOF, TOTAL_N, splitType = "quantile")
  alloc_zeroQ    <- get_neyman_allocation(df_sampling, group_customQuantile, percentTOF, TOTAL_N, splitType = "zero_Quantile")
  alloc_srs      <- get_neyman_allocation(df_sampling, group_srs, percentTOF, TOTAL_N, splitType = "SRS")
  
  # Update the list to run simulations on ALL 5 methods
  data_list <- list(
    kmeans       = list(data = alloc_kmeans,   name = "group_kmeans"),
    equal        = list(data = alloc_equal,    name = "group_equal"),
    quantile     = list(data = alloc_quantile, name = "group_quantile"),     # Added
    zeroQuantile = list(data = alloc_zeroQ,    name = "group_customQuantile"), # Added
    srs          = list(data = alloc_srs,      name = "group_srs")
  )
  
  
  # --- 3. Simulation ---
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
  
  # --- Export ---
  out_name <- paste0("neymanResults_MLRA", mlra_id, "_", year, "_", strat_var, ".csv")
  
  if(!dir.exists(dir_res)) dir.create(dir_res, recursive = TRUE)
  write_csv(final_results, file.path(dir_res, out_name))
  
  return(final_results)
}