# functions/neymanFunctions/fct_post_analysis.R

#' Rank Neyman Candidates (Excluding SRS)
#' 
#' @param final_output The tibble object from master_workflow
#' @return A summary dataframe ranked by lowest necessary sample size
rank_neyman_candidates <- function(final_output) {
  
  process_row <- function(res_df, data_df, mlra, year) {
    total_pop <- nrow(data_df)
    
    # Calculate viability for ALL methods
    viability <- get_viability_thresholds(
      results_df = res_df,
      total_population = total_pop,
      acc_threshold = 90, 
      cov_threshold = 90
    )
    
    # Filter OUT 'group_srs' so it can never be the "Winner"
    # Rank the remaining 4 Neyman methods by efficiency
    viability %>%
      dplyr::filter(method != "group_srs") %>% 
      dplyr::mutate(
        mlra = mlra,
        year = year,
        efficiency_rank = rank(pct_of_population, ties.method = "first")
      ) %>%
      dplyr::select(mlra, year, method, efficiency_rank, pct_of_population, raw_samples, run_number, everything())
  }
  
  purrr::pmap_dfr(list(final_output$sampling_results, final_output$data_clean, final_output$mlra, final_output$year),
                  process_row) %>%
    dplyr::arrange(mlra, year, efficiency_rank)
}


#' Run Three-Way Comparative Benchmark
#' 
#' Compares: 1. Top Neyman Method (The Winner), 2. Systematic SRS, 3. Pure Random
#' 
#' @param final_output The master output object
#' @param ranking_df The output from rank_neyman_candidates()
#' @param strat_col The name of the column used for stratification (REQUIRED)
compare_winner_vs_baselines <- function(final_output, ranking_df, strat_col) {
  
  # Select only the #1 Rank method for each MLRA/Year
  winners <- ranking_df %>%
    dplyr::filter(efficiency_rank == 1)
  
  # Function to process each winner row
  run_benchmark_row <- function(m_id, y_id, row_data, row_results) {
    
    # 1. Identify the Winner
    winner_info <- winners %>% dplyr::filter(mlra == m_id, year == y_id)
    if(nrow(winner_info) == 0) return(NULL)
    
    winning_method <- winner_info$method
    budget_N       <- winner_info$raw_samples
    budget_run_idx <- winner_info$run_number
    
    message("Benchmarking MLRA ", m_id, "-", y_id, ": Winner is ", winning_method, " (N=", budget_N, ") using ", strat_col)
    
    # --- A. RETRIEVE WINNER STATS ---
    res_winner <- data.frame(
      Type = "1. Stratified (Winner)",
      Method = winning_method,
      Sample_Size = budget_N,
      Accuracy = winner_info$accuracy_at_min,
      Coverage = winner_info$coverage_at_min
    )
    
    # --- B. RETRIEVE SYSTEMATIC SRS BASELINE ---
    srs_baseline <- row_results %>%
      dplyr::filter(method == "group_srs", runNumber == budget_run_idx)
    
    if(nrow(srs_baseline) == 0) { acc_srs <- NA; cov_srs <- NA } 
    else { acc_srs <- srs_baseline$percentTRUE; cov_srs <- srs_baseline$percentCOVERED }
    
    res_srs <- data.frame(
      Type = "2. Systematic Baseline",
      Method = "group_srs",
      Sample_Size = budget_N,
      Accuracy = acc_srs,
      Coverage = cov_srs
    )
    
    # --- C. RUN PURE RANDOM VALIDATION (New Simulation) ---
    df_sampling <- row_data %>%
      dplyr::select(gridID, percentTOF, areas = gridArea, strat_var = dplyr::all_of(strat_col)) %>%
      dplyr::filter(is.finite(strat_var)) %>%
      dplyr::mutate(group_srs = 1)
    
    alloc_dummy <- get_neyman_allocation(df_sampling, group_srs, percentTOF, 100, splitType = "SRS")
    totalArea <- sum(df_sampling$areas, na.rm = TRUE)
    trueVal   <- sum((df_sampling$percentTOF/100) * df_sampling$areas, na.rm=TRUE)
    multiplier <- budget_N / 100
    
    sim_out <- run_random_validation(
      data = df_sampling,
      allocation_df = alloc_dummy,
      total_area = totalArea,
      iterations = 100, 
      seed = 999, 
      nth = multiplier, 
      true_value = trueVal
    )
    
    acc_rnd <- (sum(sim_out$tofEstimate >= (trueVal*0.9) & sim_out$tofEstimate <= (trueVal*1.1)) / 100) * 100
    cov_rnd <- (sum(sim_out$isCovered, na.rm=TRUE) / 100) * 100
    
    res_random <- data.frame(
      Type = "3. Pure Random Baseline",
      Method = "run_random_validation",
      Sample_Size = budget_N,
      Accuracy = acc_rnd,
      Coverage = cov_rnd
    )
    
    dplyr::bind_rows(res_winner, res_srs, res_random) %>%
      dplyr::mutate(mlra = m_id, year = y_id) %>%
      dplyr::select(mlra, year, Type, Method, Sample_Size, Accuracy, Coverage)
  }
  
  purrr::pmap_dfr(list(final_output$mlra, final_output$year, final_output$data_clean, final_output$sampling_results),
                  run_benchmark_row)
}


#' Summarize Sample Requirements (Rolling Window Stability)
#' 
#' Uses a "Forward Window" approach to define stability.
#' A sample size is stable if it AND the next 'window_size' runs all pass.
#' 
#' @param final_output The master object
#' @param strat_col The name of the column used for stratification (REQUIRED)
#' @param window_size Number of future steps that must also pass (default 5)
#' @return A summary dataframe in wide format (Metrics in rows)
summarize_sample_requirements <- function(final_output, strat_col, window_size = 5) {
  
  # --- HELPER: Rolling Window Check ---
  get_dual_metrics <- function(df, total_pop, win) {
    # 1. Sort and define pass/fail
    processed <- df %>%
      dplyr::arrange(runNumber) %>%
      dplyr::mutate(pass = percentTRUE >= 90 & percentCOVERED >= 90)
    
    # 2. First Hit
    first_hit <- processed %>% dplyr::filter(pass) %>% dplyr::slice(1) %>% dplyr::pull(runNumber)
    first_val <- if(length(first_hit) == 0) NA else first_hit * 100
    
    # 3. Stable Hit (Rolling Window)
    n_rows <- nrow(processed)
    is_stable <- rep(FALSE, n_rows)
    
    limit <- n_rows - win
    if (limit > 0) {
      for(i in 1:limit) {
        window <- processed$pass[i : (i + win)]
        if(all(window)) {
          is_stable[i] <- TRUE
        }
      }
    }
    
    stable_idx <- which(is_stable)[1]
    stable_val <- if(is.na(stable_idx)) NA else processed$runNumber[stable_idx] * 100
    
    return(list(First = first_val, Stable = stable_val))
  }
  
  
  # --- MAIN PROCESSOR ---
  process_row <- function(m_id, y_id, row_data, row_results) {
    
    message("Calculating Rolling Stability (Window=", window_size, ") for MLRA ", m_id, " - ", y_id)
    total_pop <- nrow(row_data)
    
    # 1. PROCESS EXISTING METHODS
    summary_5 <- row_results %>%
      dplyr::group_by(method) %>%
      dplyr::summarise(
        metrics = list(get_dual_metrics(dplyr::pick(dplyr::everything()), total_pop, window_size)), 
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        First = purrr::map_dbl(metrics, "First"),
        Stable = purrr::map_dbl(metrics, "Stable")
      ) %>%
      dplyr::select(-metrics)
    
    
    # 2. GENERATE PURE RANDOM (Simulated)
    # DYNAMIC STRAT VARIABLE: Using all_of(strat_col)
    df_sampling <- row_data %>%
      dplyr::select(gridID, percentTOF, areas = gridArea, strat_var = dplyr::all_of(strat_col)) %>%
      dplyr::filter(is.finite(strat_var)) %>%
      dplyr::mutate(group_dummy = 1)
    
    totalArea <- sum(df_sampling$areas, na.rm = TRUE)
    trueVal   <- sum((df_sampling$percentTOF/100) * df_sampling$areas, na.rm=TRUE)
    alloc_dummy <- get_neyman_allocation(df_sampling, group_dummy, percentTOF, 100, splitType = "SRS")
    
    # Run Random Simulation (Cap at 100 iterations / 10,000 samples)
    runMax <- min(floor(nrow(df_sampling) / 100), 100)
    
    random_results_df <- purrr::map_dfr(1:runMax, function(i) {
      sim_out <- run_random_validation(
        data = df_sampling,
        allocation_df = alloc_dummy,
        total_area = totalArea,
        iterations = 50, # 50 sims for speed
        seed = 1000 + i, 
        nth = i,          
        true_value = trueVal
      )
      
      data.frame(
        runNumber = i,
        percentTRUE = (sum(sim_out$tofEstimate >= (trueVal*0.9) & sim_out$tofEstimate <= (trueVal*1.1)) / 50) * 100,
        percentCOVERED = (sum(sim_out$isCovered, na.rm=TRUE) / 50) * 100
      )
    })
    
    rand_metrics <- get_dual_metrics(random_results_df, total_pop, window_size)
    
    
    # 3. COMBINE & RESHAPE
    bind_rows(
      summary_5,
      data.frame(method = "Pure_Random", First = rand_metrics$First, Stable = rand_metrics$Stable)
    ) %>%
      tidyr::pivot_longer(
        cols = c(First, Stable),
        names_to = "Metric",
        values_to = "Sample_Size"
      ) %>%
      tidyr::pivot_wider(
        names_from = method,
        values_from = Sample_Size
      ) %>%
      dplyr::mutate(mlra = m_id, year = y_id) %>%
      dplyr::select(mlra, year, Metric, dplyr::everything())
  }
  
  purrr::pmap_dfr(list(final_output$mlra, final_output$year, final_output$data_clean, final_output$sampling_results),
                  process_row)
}