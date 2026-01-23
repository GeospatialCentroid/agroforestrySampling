# ==============================================================================
# 03_neyman_allocation.R
# Purpose: Run Neyman Optimal Allocation analysis on processed data.
#          1. Tests multiple stratification methods (K-Means, Zero-KM, Quantile, Zero-Q).
#          2. Automatically merges strata that are too small (<50 grids).
#          3. Generates visualizations of top 10 performers per year.
#          4. Outputs allocation tables and summary statistics.
# ==============================================================================

source("scripts/neyman2/00_config.r")

# --- 1. CONFIGURATION & OUTPUTS -----------------------------------------------

OUTPUT_ALLOC_DIR <- file.path(DERIVED_DIR, "neyman_allocations")
if (!dir.exists(OUTPUT_ALLOC_DIR)) {
  dir.create(OUTPUT_ALLOC_DIR, recursive = TRUE)
}

# Define Stratification Parameters to Test
STRAT_VARS <- c(
  "riparian_pct",
  "Water",
  "Developed",
  "Barren",
  "Forest",
  "Shrubland",
  "Herbaceous",
  "Cultivated",
  "Wetlands"
)

# Define Methods to Test
# Removed 'equal_interval', Added 'zero_kmeans'
METHODS <- c("kmeans", "zero_kmeans", "quantile", "zero_quantile")
STRATA_COUNTS <- c(3, 4, 5)


# --- 2. HELPER FUNCTIONS ------------------------------------------------------

#' Merge Small Strata
#'
#' Iteratively merges the smallest stratum into its most similar neighbor
#' until all strata meet the minimum size threshold.
#'
#' @param df Dataframe containing 'strata' and the target variable column.
#' @param target_col Name of the column used for calculating means (e.g., 'Forest').
#' @param min_size Minimum number of grid cells required per stratum.
#' @return Dataframe with updated 'strata' column.
merge_small_strata <- function(df, target_col, min_size = 50) {
  # Loop until condition is met
  while (TRUE) {
    # 1. Calculate Stratum Sizes
    counts <- df %>%
      dplyr::count(strata) %>%
      dplyr::arrange(n)

    # 2. Check Exit Conditions
    if (nrow(counts) == 0) {
      return(df)
    } # Safety for empty df
    if (min(counts$n) >= min_size) {
      return(df)
    } # Smallest is big enough
    if (nrow(counts) <= 1) {
      return(df)
    } # Can't merge if only 1 left

    # 3. Identify the "Problem" Stratum (The smallest one)
    small_strat_id <- counts$strata[1]

    # 4. Find Best Merge Candidate
    # We want to merge with the neighbor (ID +/- 1) that has the closest Mean value.
    strat_means <- df %>%
      dplyr::group_by(strata) %>%
      dplyr::summarise(
        avg_val = mean(!!rlang::sym(target_col), na.rm = TRUE),
        .groups = "drop"
      )

    current_mean <- strat_means$avg_val[strat_means$strata == small_strat_id]

    # Identify potential neighbors (Previous and Next indices present in data)
    # We check strat_means$strata to handle cases where gaps exist (e.g. 1, 3, 4)
    sorted_ids <- sort(strat_means$strata)
    idx <- which(sorted_ids == small_strat_id)

    neighbor_ids <- c()
    if (idx > 1) {
      neighbor_ids <- c(neighbor_ids, sorted_ids[idx - 1])
    }
    if (idx < length(sorted_ids)) {
      neighbor_ids <- c(neighbor_ids, sorted_ids[idx + 1])
    }

    neighbors <- strat_means %>% dplyr::filter(strata %in% neighbor_ids)

    if (nrow(neighbors) == 0) {
      return(df)
    }

    # Pick the neighbor with the absolute closest mean value
    target_strat_id <- neighbors$strata[which.min(abs(
      neighbors$avg_val - current_mean
    ))]

    # 5. Execute Merge
    df$strata[df$strata == small_strat_id] <- target_strat_id

    # 6. Re-Index Strata (1, 2, 3...)
    df$strata <- as.numeric(as.factor(df$strata))
  }
}

#' Calculate Neyman Statistics for a Stratification
calculate_neyman_stats <- function(df) {
  stats <- df %>%
    dplyr::group_by(strata) %>%
    dplyr::summarise(
      Nh = n(),
      Sh = sd(TOF, na.rm = TRUE),
      Mean = mean(TOF, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Sh = ifelse(is.na(Sh), 0, Sh),
      Nh_Sh = Nh * Sh
    )

  optimization_metric <- sum(stats$Nh_Sh)

  return(data.frame(
    metric_score = optimization_metric,
    total_grids = sum(stats$Nh),
    n_strata = nrow(stats)
  ))
}

#' Generate Plot of Top 10 Performers
plot_top_performers <- function(results_df, m_id, out_dir) {
  # Filter top 10 per year
  top_10 <- results_df %>%
    dplyr::filter(method != "none") %>%
    dplyr::group_by(year) %>%
    dplyr::slice_max(order_by = efficiency_gain_pct, n = 10) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      label = paste0(variable, " (", method, "-", k, ")"),
      # Reorder factor for plotting so bars are sorted
      label = factor(label, levels = unique(label[order(efficiency_gain_pct)]))
    )

  p <- ggplot2::ggplot(
    top_10,
    aes(x = efficiency_gain_pct, y = label, fill = as.factor(year))
  ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::facet_wrap(~year, scales = "free_y", ncol = 1) +
    ggplot2::labs(
      title = paste("Top 10 Stratification Methods: MLRA", m_id),
      subtitle = "Ranked by Efficiency Gain (%) over Random Sampling",
      x = "Efficiency Gain (%)",
      y = "Stratification Strategy"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = element_text(size = 8),
      strip.background = element_rect(fill = "lightgrey", color = NA)
    )

  ggsave(
    filename = file.path(out_dir, paste0("MLRA_", m_id, "_top_performers.png")),
    plot = p,
    width = 8,
    height = 10,
    bg = "white"
  )
}

#' Apply Stratification Logic
apply_stratification <- function(data, variable, method, k) {
  vals <- data[[variable]]

  # Safety: If variable is essentially constant/empty
  if (sum(vals, na.rm = TRUE) == 0 || var(vals, na.rm = TRUE) == 0) {
    return(NULL)
  }

  out_df <- data

  # --- METHOD 1: QUANTILE ---
  if (method == "quantile") {
    if (dplyr::n_distinct(vals) < k) {
      return(NULL)
    }
    tryCatch(
      {
        out_df$strata <- as.numeric(cut(
          vals,
          breaks = quantile(
            vals,
            probs = seq(0, 1, length.out = k + 1),
            na.rm = TRUE
          ),
          include.lowest = TRUE,
          labels = FALSE
        ))
      },
      error = function(e) return(NULL)
    )

    # --- METHOD 2: K-MEANS ---
  } else if (method == "kmeans") {
    tryCatch(
      {
        km <- kmeans(vals, centers = k, nstart = 10)
        rank_map <- rank(km$centers)
        out_df$strata <- rank_map[km$cluster]
      },
      error = function(e) return(NULL)
    )

    # --- METHOD 3: ZERO-INFLATED QUANTILE ---
  } else if (method == "zero_quantile") {
    if (k < 2) {
      return(NULL)
    }
    if (!any(vals == 0)) {
      return(apply_stratification(data, variable, "quantile", k))
    }

    tryCatch(
      {
        out_df$strata <- NA
        out_df$strata[vals == 0] <- 1

        non_zeros <- vals[vals > 0]
        if (length(non_zeros) > 0) {
          bins <- k - 1
          if (dplyr::n_distinct(non_zeros) >= bins) {
            nz_breaks <- quantile(
              non_zeros,
              probs = seq(0, 1, length.out = bins + 1),
              na.rm = TRUE
            )
            nz_strata <- as.numeric(cut(
              non_zeros,
              breaks = nz_breaks,
              include.lowest = TRUE,
              labels = FALSE
            ))
            out_df$strata[vals > 0] <- nz_strata + 1
          } else {
            return(NULL)
          }
        }
      },
      error = function(e) return(NULL)
    )

    # --- METHOD 4: ZERO-INFLATED K-MEANS ---
  } else if (method == "zero_kmeans") {
    if (k < 2) {
      return(NULL)
    }
    if (!any(vals == 0)) {
      return(apply_stratification(data, variable, "kmeans", k))
    }

    tryCatch(
      {
        out_df$strata <- NA
        out_df$strata[vals == 0] <- 1

        non_zeros <- vals[vals > 0]
        if (length(non_zeros) > 0) {
          bins <- k - 1
          # Run K-Means only on non-zeros
          km <- kmeans(non_zeros, centers = bins, nstart = 10)
          rank_map <- rank(km$centers) # 1..bins

          # Shift ranks by +1 so they start at 2
          out_df$strata[vals > 0] <- rank_map[km$cluster] + 1
        }
      },
      error = function(e) return(NULL)
    )
  }

  if ("strata" %in% names(out_df) && !any(is.na(out_df$strata))) {
    return(out_df)
  } else {
    return(NULL)
  }
}


# --- 3. MAIN LOOP -------------------------------------------------------------

if (!is.null(TARGET_MLRA_IDS)) {
  mlra_ids <- TARGET_MLRA_IDS
} else {
  mlra_ids <- ALL_MLRA_IDS
}

message(paste("Processing Allocation for:", paste(mlra_ids, collapse = ", ")))

for (m_id in mlra_ids) {
  message(paste0("\n--- Analyzing MLRA: ", m_id, " ---"))

  input_file <- file.path(
    DERIVED_DIR,
    "dynamic_attributes",
    paste0("MLRA_", m_id, "_master_dataset.csv")
  )
  if (!file.exists(input_file)) {
    warning(paste("Master dataset missing for MLRA", m_id))
    next
  }

  df_long <- readr::read_csv(input_file, show_col_types = FALSE)

  if (!"TOF" %in% names(df_long)) {
    stop("Error: Column 'TOF' not found in dataset.")
  }

  # B. Iterate: Year -> Variable -> Method -> K
  results_list <- list()
  years <- unique(df_long$year)

  for (yr in years) {
    message(paste0("   Testing Year: ", yr))
    df_yr <- df_long %>% dplyr::filter(year == yr)
    if (nrow(df_yr) == 0) {
      next
    }

    # 1. Baseline
    base_stats <- tryCatch(
      {
        df_yr %>% dplyr::mutate(strata = 1) %>% calculate_neyman_stats()
      },
      error = function(e) NULL
    )

    if (is.null(base_stats)) {
      next
    }
    baseline_score <- base_stats$metric_score

    # Log Baseline
    results_list[[length(results_list) + 1]] <- data.frame(
      year = yr,
      variable = "BASELINE_RANDOM",
      method = "none",
      k = 1,
      metric_score = baseline_score,
      efficiency_gain_pct = 0
    )

    # 2. Test Stratifiers
    for (var_name in STRAT_VARS) {
      if (!var_name %in% names(df_yr)) {
        next
      }

      for (meth in METHODS) {
        for (k in STRATA_COUNTS) {
          # Skip invalid combos
          if ((meth == "zero_quantile" || meth == "zero_kmeans") && k < 2) {
            next
          }

          # 2a. Apply Stratification
          df_strat <- apply_stratification(df_yr, var_name, meth, k)
          if (is.null(df_strat)) {
            next
          }

          # 2b. SAFETY: Merge Small Strata (Important for K-Means/Zero-KM)
          # Only run this for K-Means variants where small groups are uncontrolled
          if (meth %in% c("kmeans", "zero_kmeans")) {
            df_strat <- merge_small_strata(
              df_strat,
              target_col = var_name,
              min_size = 50
            )
          }

          # 2c. Calculate Stats
          stats <- calculate_neyman_stats(df_strat)
          gain <- (baseline_score - stats$metric_score) / baseline_score * 100

          results_list[[length(results_list) + 1]] <- data.frame(
            year = yr,
            variable = var_name,
            method = meth,
            k = k,
            metric_score = stats$metric_score,
            efficiency_gain_pct = round(gain, 2)
          )
        }
      }
    }
  }

  # C. Process & Save
  if (length(results_list) == 0) {
    message("   No valid results generated for this MLRA.")
    next
  }

  results_df <- dplyr::bind_rows(results_list) %>%
    dplyr::arrange(year, desc(efficiency_gain_pct))

  summary_file <- file.path(
    OUTPUT_ALLOC_DIR,
    paste0("MLRA_", m_id, "_allocation_summary.csv")
  )
  readr::write_csv(results_df, summary_file)

  # Generate Top 10 Plot
  message("   Generating performance plot...")
  plot_top_performers(results_df, m_id, OUTPUT_ALLOC_DIR)

  # D. Save Best Allocation
  best_methods <- results_df %>%
    dplyr::group_by(year) %>%
    dplyr::slice(1)

  message("   Top Stratifiers found:")
  print(
    best_methods %>%
      dplyr::select(year, variable, method, k, efficiency_gain_pct)
  )

  final_allocations <- list()

  for (yr in years) {
    best <- best_methods %>% dplyr::filter(year == yr)
    if (nrow(best) == 0 || best$variable == "BASELINE_RANDOM") {
      next
    }

    df_yr <- df_long %>% dplyr::filter(year == yr)

    # Re-Apply logic
    df_final <- apply_stratification(df_yr, best$variable, best$method, best$k)

    # Re-Apply Safety Merge if needed
    if (best$method %in% c("kmeans", "zero_kmeans")) {
      df_final <- merge_small_strata(
        df_final,
        target_col = best$variable,
        min_size = 50
      )
    }

    strata_stats <- df_final %>%
      dplyr::group_by(strata) %>%
      dplyr::summarise(
        Nh = n(),
        Sh = sd(TOF, na.rm = TRUE),
        Mean_TOF = mean(TOF, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        allocation_weight = (Nh * Sh) / sum(Nh * Sh)
      )

    df_export <- df_final %>%
      dplyr::left_join(strata_stats, by = "strata") %>%
      dplyr::mutate(
        optimal_variable = best$variable,
        optimal_method = best$method
      ) %>%
      dplyr::select(
        id,
        MLRA_ID,
        year,
        TOF,
        optimal_variable,
        optimal_method,
        strata,
        allocation_weight,
        Mean_TOF,
        Nh,
        Sh
      )

    final_allocations[[as.character(yr)]] <- df_export
  }

  if (length(final_allocations) > 0) {
    all_allocs <- dplyr::bind_rows(final_allocations)
    alloc_file <- file.path(
      OUTPUT_ALLOC_DIR,
      paste0("MLRA_", m_id, "_optimal_allocations.csv")
    )
    readr::write_csv(all_allocs, alloc_file)
    message(paste0("   Saved Allocations: ", alloc_file))
  }
}

message("\nNeyman Allocation analysis complete.")
