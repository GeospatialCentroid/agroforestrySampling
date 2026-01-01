# R/fct_sampling.R

#' Calculate Neyman Allocation (Robust)
get_neyman_allocation <- function(data, strata_col, target_col, total_n, min_n = 4, splitType) {
  
  # 1. Calculate Statistics per Stratum
  stats_df <- data |>
    dplyr::group_by({{ strata_col }}) |>
    dplyr::summarise(
      Nh = dplyr::n(),
      # Handle NA in sd() if stratum has only 1 observation
      Sh = sd({{ target_col }}, na.rm = TRUE),
      mean_target = mean({{ target_col }}, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      # If Sh is NA (1 observation) or NaN, replace with 0
      Sh = tidyr::replace_na(Sh, 0)
    )
  
  # 2. Calculate Weights (Neyman: weight ~ Nh * Sh)
  # Check for edge case: If ALL strata have 0 variance (sum(product) == 0),
  # we cannot divide by zero. Fallback to Proportional Allocation (weight ~ Nh).
  
  result <- stats_df |>
    dplyr::mutate(
      product = Nh * Sh,
      
      allocation_weight = dplyr::case_when(
        sum(product, na.rm = TRUE) == 0 ~ Nh / sum(Nh), # Fallback: Proportional
        TRUE ~ product / sum(product, na.rm = TRUE)     # Standard Neyman
      ),
      
      # 3. Calculate Sample Sizes
      n_raw = round(total_n * allocation_weight),
      
      # 4. Apply Constraints
      # Ensure min_n doesn't exceed the actual population (Nh)
      n_final = pmax(n_raw, min_n),
      n_final = pmin(n_final, Nh),
      
      # 5. Metadata
      density = n_final / Nh,
      split = splitType
    ) |>
    # Fill any lingering NAs in n_final with min_n (just in case)
    dplyr::mutate(n_final = tidyr::replace_na(n_final, min(min_n, Nh))) |> 
    dplyr::select(group = {{ strata_col }}, dplyr::everything(), -product)
  
  return(result)
}

#' Get Circular Indices for Systematic Sampling
get_circular_indices <- function(start, n_sample, total_rows) {
  # 1. Calculate the precise fractional step size
  step_size <- total_rows / n_sample
  
  # 2. Generate fractional positions
  fractional_seq <- (0:(n_sample - 1)) * step_size
  # 3. Add random start and round to nearest integer
  raw_indices <- round(fractional_seq + start)
  
  # 4. Wrap around using Modulo
  final_indices <- (raw_indices - 1) %% total_rows + 1
  return(unique(final_indices))
}

#' Adjust Sample Size Multiplier
increase_sample_size <- function(data, multiplier, total_population_count) {
  
  # 1. Calculate the target total sample size for this iteration
  # e.g. if multiplier is 10, we want 10 * 100 = 1000 samples total
  target_total_n <- 100 * multiplier 
  
  data |>
    dplyr::mutate(
      # 2. Recalculate allocation for this NEW total size
      n_raw = round(target_total_n * allocation_weight),
      # 3. Apply constraints (min 2, max Nh)
      n_final = pmax(n_raw, 2),
      n_final = pmin(n_final, Nh)
    )
}

#' CORRECTED SIMULATION: With Confidence Interval & Coverage Check
run_neyman_simulation <- function(data, allocation_df, total_area, iterations, seed = 1234, nth) {
  set.seed(seed)
  
  # 1. Setup Data
  allocation_df <- increase_sample_size(data = allocation_df, multiplier = nth)
  
  grouped_list <- data %>%
    dplyr::inner_join(allocation_df %>% dplyr::select(group, n_final), by = "group") %>%
    dplyr::group_split(group)
  
  # 2. Simulation Loop
  results <- purrr::map_dfr(1:iterations, function(i) {
    
    # --- STRATUM LEVEL CALCULATIONS ---
    stratum_stats <- purrr::map_dfr(grouped_list, function(group_df) {
      target_n <- group_df$n_final[1]
      total_N_pop <- nrow(group_df)
      total_Area_pop <- sum(group_df$areas, na.rm = TRUE)
      
      # Systematic Selection (using the Fractional Step fix)
      start_node <- sample(1:total_N_pop, 1)
      idx <- get_circular_indices(start = start_node, n_sample = target_n, total_rows = total_N_pop)
      sample_subset <- group_df[idx, ]
      
      # A. Point Estimate (The Total TOF Area for this stratum)
      mean_prop <- weighted.mean(x = sample_subset$percentTOF, w = sample_subset$areas, na.rm = TRUE) / 100
      est_total_h <- mean_prop * total_Area_pop
      
      # B. Variance Estimate (For Confidence Intervals)
      # Calculate sample variance of the raw % data (converted to proportion)
      # Handle single-sample edge case (returns 0 variance if n < 2, though min_n should prevent this)
      if(target_n < 2) {
        var_h <- 0 
      } else {
        var_h <- var(sample_subset$percentTOF / 100, na.rm = TRUE)
      }
      
      # Finite Population Correction (FPC): (1 - n/N)
      # If we sample everyone (n=N), FPC is 0, so Variance is 0.
      fpc <- 1 - (target_n / total_N_pop)
      
      # Variance contribution of this stratum: A^2 * (1-f) * (s^2 / n)
      var_contribution <- (total_Area_pop^2) * fpc * (var_h / target_n)
      
      data.frame(
        est_total_h = est_total_h,
        var_contribution = var_contribution
      )
    })
    
    # --- GLOBAL LEVEL CALCULATIONS ---
    global_estimate <- sum(stratum_stats$est_total_h)
    global_variance <- sum(stratum_stats$var_contribution)
    global_se <- sqrt(global_variance)
    
    # Calculate 95% Confidence Interval
    ci_lower <- global_estimate - (1.96 * global_se)
    ci_upper <- global_estimate + (1.96 * global_se)
    
    data.frame(
      iteration = i,
      tofEstimate = global_estimate,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      # Did the CI capture the true total? (We need true_total passed to this function, 
      # or we can check it later if we return the CI cols)
      std_error = global_se
    )
  })
  
  return(results)
}



# run the neyman allocation method 
runSampling_neyman <- function(data_list, tofDF, totalArea, runIterations, low, high, true_value) {
  
  method <- data_list$name
  valsDF <- data_list$data
  runMax <- floor(nrow(tofDF) / 100)
  
  # Prepare data
  df2 <- tofDF |>
    dplyr::select(gridID, percentTOF, strat_var, group = {{ method }}, areas)
  
  resultsDF <- data.frame(
    runNumber = 1:runMax,
    percentTRUE = NA,     # Hit Rate (Your original metric)
    percentCOVERED = NA,  # Coverage Probability (New metric)
    method = method
  )
  
  for (i in 1:runMax) {
    # Run Simulation
    outputVals <- run_neyman_simulation(
      data = df2,
      allocation_df = valsDF,
      total_area = totalArea,
      iterations = runIterations,
      nth = i
    )
    
    # A. Check Hit Rate (Is estimate within +/- 10% of truth?)
    ranges <- outputVals |>
      dplyr::mutate(
        inRange = case_when(
          tofEstimate >= low & tofEstimate <= high ~ TRUE,
          TRUE ~ FALSE
        ),
        # B. Check Coverage (Is Truth inside the 95% CI?)
        isCovered = case_when(
          true_value >= ci_lower & true_value <= ci_upper ~ TRUE,
          TRUE ~ FALSE
        )
      )
    
    resultsDF$percentTRUE[i]    <- (sum(ranges$inRange) / runIterations) * 100
    resultsDF$percentCOVERED[i] <- (sum(ranges$isCovered) / runIterations) * 100
  }
  
  return(resultsDF)
}

#' VALIDATION: Pure Simple Random Sampling (SRS)
#' Ignores all grouping/stratification. Just grabs N random grids.
#' Used to baseline the performance of stratified methods.
run_random_validation <- function(data, allocation_df, total_area, iterations, seed = 999, nth, true_value = NULL) {
  set.seed(seed)
  
  # 1. Calculate Total Sample Size (Budget)
  # We use the allocation_df only to determine HOW MANY samples we can afford
  # so that we compare this against the stratified method using the exact same sample size.
  allocation_step <- increase_sample_size(data = allocation_df, multiplier = nth)
  total_sample_size <- sum(allocation_step$n_final, na.rm = TRUE)
  
  # Safety: Don't sample more than the population
  total_N_pop <- nrow(data)
  if (total_sample_size > total_N_pop) total_sample_size <- total_N_pop
  
  # 2. Simulation Loop
  results <- purrr::map_dfr(1:iterations, function(i) {
    
    # --- PURE RANDOM SELECTION ---
    # No splitting by group. Just slice from the main dataframe.
    sample_subset <- data %>%
      dplyr::slice_sample(n = total_sample_size, replace = FALSE)
    
    # 3. Point Estimate (Global)
    # Weighted mean of the sample scaled to total area
    # (Assuming NA values in percentTOF are missing data, handled by na.rm)
    mean_prop <- weighted.mean(x = sample_subset$percentTOF, w = sample_subset$areas, na.rm = TRUE) / 100
    
    if(is.nan(mean_prop) || is.na(mean_prop)) mean_prop <- 0
    
    est_total <- mean_prop * total_area
    
    # 4. Variance Estimate (SRS Formula)
    # Extract valid values to check effective sample size
    valid_vals <- sample_subset$percentTOF[!is.na(sample_subset$percentTOF)]
    n_eff <- length(valid_vals)
    
    if(n_eff < 2) {
      global_se <- 0
    } else {
      # Sample variance of the proportion (raw variance of the % values converted to decimal)
      var_sample <- var(valid_vals / 100)
      
      # Finite Population Correction (FPC) for SRS
      fpc <- 1 - (total_sample_size / total_N_pop)
      fpc <- max(0, fpc)
      
      # Standard Error of the Total Estimate
      # Formula: Total_Area * sqrt( (1-f) * (s^2 / n) )
      # Note: We use Total_Area as the scaler because we are estimating a density (ratio) and scaling up.
      global_se <- total_area * sqrt( fpc * (var_sample / total_sample_size) )
    }
    
    # 5. CI & Coverage
    ci_lower <- est_total - (1.96 * global_se)
    ci_upper <- est_total + (1.96 * global_se)
    
    is_covered <- NA
    if (!is.null(true_value)) {
      is_covered <- (true_value >= ci_lower & true_value <= ci_upper)
    }
    
    data.frame(
      iteration = i,
      tofEstimate = est_total,
      std_error = global_se,
      isCovered = is_covered
    )
  })
  
  return(results)
}
