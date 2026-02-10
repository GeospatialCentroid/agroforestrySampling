# ==============================================================================
# 06_forecast_2026_design.R
# Purpose: Forecast sampling needs for the upcoming 2026 field season.
#          1. Analyzes stability of strata weights across 2010-2020.
#          2. Calculates "Robust Weights" (Max historical weight per stratum).
#          3. Generates a physical sample using 2020 strata + Projected Weights.
# ==============================================================================

source("scripts/neyman2/00_config.r")

# --- 1. SETTINGS --------------------------------------------------------------

OUTPUT_FORECAST_DIR <- file.path(DERIVED_DIR, "forecast_2026")
if (!dir.exists(OUTPUT_FORECAST_DIR)) {
  dir.create(OUTPUT_FORECAST_DIR, recursive = TRUE)
}

TARGET_SAMPLE_SIZE <- 300 # Target for 2026
MIN_STRATA_SIZE <- 5
OVERSAMPLE_PCT <- 0.25
BASE_YEAR_MAP <- 2020 # Use the most recent map for physical locations

# --- 2. HELPER FUNCTIONS ------------------------------------------------------

calculate_n_h <- function(df, N_total, min_n) {
  df <- df %>%
    dplyr::mutate(
      raw_n = round(robust_weight * N_total), # Use ROBUST weight
      n_h = pmax(raw_n, min_n),
      n_h = pmin(n_h, Nh)
    )

  diff <- N_total - sum(df$n_h)
  if (diff != 0) {
    adj_idx <- which.max(df$n_h)
    if (df$n_h[adj_idx] + diff <= df$Nh[adj_idx]) {
      df$n_h[adj_idx] <- df$n_h[adj_idx] + diff
    }
  }
  setNames(as.integer(df$n_h), df$strata_id)
}

# --- 3. MAIN LOOP -------------------------------------------------------------

if (!is.null(TARGET_MLRA_IDS)) {
  mlra_ids <- TARGET_MLRA_IDS
} else {
  mlra_ids <- ALL_MLRA_IDS
}

message(paste("Forecasting 2026 Design for:", paste(mlra_ids, collapse = ", ")))

for (m_id in mlra_ids) {
  message(paste0("\n=== Forecasting MLRA: ", m_id, " ==="))

  alloc_file <- file.path(
    DERIVED_DIR,
    "neyman_allocations",
    paste0("MLRA_", m_id, "_optimal_allocations.csv")
  )
  grid_file <- STATIC_INPUTS$grid_1km

  if (!file.exists(alloc_file)) {
    warning(paste("Allocation file missing for MLRA", m_id))
    next
  }

  # --- A. Analyze Historical Trends ---
  alloc_df <- readr::read_csv(alloc_file, show_col_types = FALSE)

  # Check if the "Optimal Variable" is consistent
  best_vars <- alloc_df %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(var = first(optimal_variable))

  message("   Historical Stratifiers:")
  print(best_vars)

  # --- B. Calculate Robust Weights ---
  # We group by Stratum and take the MAXIMUM weight seen in any year.
  # This ensures we don't under-sample a stratum that has a history of high variance.
  robust_stats <- alloc_df %>%
    dplyr::mutate(strata_id = paste0("Stratum_", strata)) %>%
    dplyr::group_by(strata_id) %>%
    dplyr::summarise(
      Nh = dplyr::first(Nh[year == BASE_YEAR_MAP]), # Use 2020 Population Count
      max_weight = max(allocation_weight, na.rm = TRUE),
      mean_weight = mean(allocation_weight, na.rm = TRUE),
      variability = sd(allocation_weight, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      # Renormalize max weights so they sum to 1
      robust_weight = max_weight / sum(max_weight)
    )

  message("   Projected 2026 Weights (Robust):")
  print(robust_stats %>% dplyr::select(strata_id, max_weight, robust_weight))

  # --- C. Generate Spatial Sample (Using 2020 Map + 2026 Weights) ---
  message("   Generating 2026 Sample Points...")

  # 1. Load 2020 Map Data
  # We use 2020 allocations to define the "Strata" boundaries for the physical map
  map_df <- alloc_df %>%
    dplyr::filter(year == BASE_YEAR_MAP) %>%
    dplyr::mutate(strata_id = paste0("Stratum_", strata))

  if (nrow(map_df) == 0) {
    warning("Base year map (2020) missing!")
    next
  }

  # 2. Load Geometry
  mlra_grid <- terra::vect(grid_file)
  mlra_grid <- mlra_grid[mlra_grid$MLRA_ID == m_id, ]
  mlra_sf <- sf::st_as_sf(mlra_grid)

  # Project
  target_crs <- if (exists("ALBERS_CRS")) ALBERS_CRS else "EPSG:5070"
  mlra_sf <- sf::st_transform(mlra_sf, crs = 5070)

  # Join
  design_sf <- mlra_sf %>% dplyr::inner_join(map_df, by = "id")

  # 3. Calculate Sample Sizes using ROBUST WEIGHTS
  n_base_vec <- calculate_n_h(robust_stats, TARGET_SAMPLE_SIZE, MIN_STRATA_SIZE)

  # Oversample
  pop_counts <- setNames(robust_stats$Nh, robust_stats$strata_id)[names(
    n_base_vec
  )]
  available <- pop_counts - n_base_vec
  n_over_vec <- pmin(pmax(round(n_base_vec * OVERSAMPLE_PCT), 1), available)
  n_over_vec[available <= 0] <- 0
  n_over_vec <- as.integer(n_over_vec)

  # 4. Run GRTS
  set.seed(2026) # Future seed

  tryCatch(
    {
      sample_design <- spsurvey::grts(
        sframe = design_sf,
        n_base = n_base_vec,
        n_over = n_over_vec,
        stratum_var = "strata_id",
        projcrs_check = FALSE
      )

      # 5. Export
      final_sites <- sample_design$sites_legacy
      if (is.null(final_sites)) {
        base <- sample_design$sites_base %>%
          dplyr::mutate(sample_type = "Primary")
        over <- sample_design$sites_over %>%
          dplyr::mutate(sample_type = "Oversample")
        final_sites <- dplyr::bind_rows(base, over)
      } else {
        final_sites <- final_sites %>%
          dplyr::mutate(
            sample_type = ifelse(panel == "OverSamp", "Oversample", "Primary")
          )
      }

      final_sites <- final_sites %>%
        dplyr::mutate(
          MLRA_ID = m_id,
          YEAR = 2026,
          NOTE = "Projected from 2010-2020 Trends"
        ) %>%
        dplyr::select(siteID, id, strata_id, sample_type, MLRA_ID, YEAR, NOTE)

      out_gpkg <- file.path(
        OUTPUT_FORECAST_DIR,
        paste0("MLRA_", m_id, "_Projected_Sample_2026.gpkg")
      )
      sf::st_write(final_sites, out_gpkg, append = FALSE, quiet = TRUE)

      # Save Stats Summary
      readr::write_csv(
        robust_stats,
        file.path(OUTPUT_FORECAST_DIR, paste0("MLRA_", m_id, "_2026_stats.csv"))
      )

      message(paste0("   Success! Saved 2026 Projection: ", basename(out_gpkg)))
    },
    error = function(e) {
      message(paste("   GRTS Error:", e$message))
    }
  )
}

message("\n2026 Forecasting Complete.")
