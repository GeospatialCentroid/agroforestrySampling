# ==============================================================================
# 04_generate_spatial_sample.R
# Purpose: Generate Spatially Balanced (GRTS) sample designs for ALL YEARS.
#          1. Loads optimal stratification for 2010, 2016, 2020.
#          2. Projects geometry once per MLRA.
#          3. Loops through years to generate year-specific designs.
# ==============================================================================

source("scripts/neyman2/00_config.r")

# --- 1. SETTINGS --------------------------------------------------------------

OUTPUT_SAMPLE_DIR <- file.path(DERIVED_DIR, "spatial_samples")
if (!dir.exists(OUTPUT_SAMPLE_DIR)) {
  dir.create(OUTPUT_SAMPLE_DIR, recursive = TRUE)
}

TARGET_SAMPLE_SIZE <- 300 # Total sites per MLRA/Year
MIN_STRATA_SIZE <- 5 # Minimum sites per stratum
OVERSAMPLE_PCT <- 0.25 # 25% extra "Spare" sites
TARGET_YEARS <- c(2010, 2016, 2020) # Process all years
IF_MISSING_CRS <- "EPSG:5070"


# --- 2. HELPER: INTEGER ALLOCATION --------------------------------------------

calculate_n_h <- function(df, N_total, min_n) {
  df <- df %>%
    dplyr::mutate(
      raw_n = round(allocation_weight * N_total),
      n_h = pmax(raw_n, min_n),
      n_h = pmin(n_h, Nh)
    )

  current_sum <- sum(df$n_h)
  diff <- N_total - current_sum

  if (diff != 0) {
    adjust_idx <- which.max(df$n_h)
    if (df$n_h[adjust_idx] + diff <= df$Nh[adjust_idx]) {
      df$n_h[adjust_idx] <- df$n_h[adjust_idx] + diff
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

message(paste("Generating Samples for:", paste(mlra_ids, collapse = ", ")))

for (m_id in mlra_ids) {
  message(paste0("\n=== Processing MLRA: ", m_id, " ==="))

  # A. Setup Shared Paths
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

  # B. Load & Project Geometry (ONCE per MLRA)
  message("   Loading and projecting geometry (Shared)...")
  mlra_grid <- terra::vect(grid_file)
  mlra_grid <- mlra_grid[mlra_grid$MLRA_ID == m_id, ]

  mlra_sf <- sf::st_as_sf(mlra_grid)
  target_crs <- if (exists("ALBERS_CRS")) ALBERS_CRS else IF_MISSING_CRS
  mlra_sf <- sf::st_transform(mlra_sf, crs = 5070)

  # C. Loop Through Years
  for (yr in TARGET_YEARS) {
    message(paste0("\n   --- Year: ", yr, " ---"))

    out_gpkg <- file.path(
      OUTPUT_SAMPLE_DIR,
      paste0("MLRA_", m_id, "_GRTS_sample_", yr, ".gpkg")
    )

    # 1. Load Allocations for this Year
    alloc_df <- readr::read_csv(alloc_file, show_col_types = FALSE) %>%
      dplyr::filter(year == yr)

    if (nrow(alloc_df) == 0) {
      warning(paste("No allocation data found for year", yr))
      next
    }

    # 2. Join Allocations to Geometry
    alloc_df <- alloc_df %>%
      dplyr::mutate(strata_id = paste0("Stratum_", strata))
    design_sf <- mlra_sf %>% dplyr::inner_join(alloc_df, by = "id")

    # 3. Calculate Sample Sizes
    strata_summary <- design_sf %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(strata_id) %>%
      dplyr::summarise(
        Nh = dplyr::first(Nh),
        allocation_weight = dplyr::first(allocation_weight)
      )

    n_base_vec <- calculate_n_h(
      strata_summary,
      TARGET_SAMPLE_SIZE,
      MIN_STRATA_SIZE
    )

    pop_counts <- setNames(strata_summary$Nh, strata_summary$strata_id)[names(
      n_base_vec
    )]

    # Oversample Logic
    available <- pop_counts - n_base_vec
    raw_over <- pmax(round(n_base_vec * OVERSAMPLE_PCT), 1)
    n_over_vec <- pmin(raw_over, available)
    n_over_vec[available <= 0] <- 0
    n_over_vec <- as.integer(n_over_vec)

    message("      Design Targets (Base / Over):")
    print(data.frame(
      Strata = names(n_base_vec),
      Base = n_base_vec,
      Over = n_over_vec
    ))

    # 4. Run GRTS
    message("      Running GRTS algorithm...")
    set.seed(1234 + yr) # Different seed per year is optional, but often good practice

    sample_design <- NULL
    tryCatch(
      {
        sample_design <- spsurvey::grts(
          sframe = design_sf,
          n_base = n_base_vec,
          n_over = n_over_vec,
          stratum_var = "strata_id",
          projcrs_check = FALSE
        )
      },
      error = function(e) {
        message(paste("      GRTS Warning/Error:", e$message))
      }
    )

    # 5. Format & Export
    if (!is.null(sample_design)) {
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

      # FIX: Re-add MLRA_ID and YEAR
      final_sites <- final_sites %>%
        dplyr::mutate(MLRA_ID = m_id, YEAR = yr) %>%
        dplyr::select(siteID, id, strata_id, sample_type, MLRA_ID, YEAR)

      sf::st_write(final_sites, out_gpkg, append = FALSE, quiet = TRUE)
      message(paste0("      Success! Saved: ", basename(out_gpkg)))
    } else {
      warning(paste("GRTS failed for MLRA", m_id, "Year", yr))
    }
  } # End Year Loop
} # End MLRA Loop

message("\nAll Years Spatial Sampling complete.")
