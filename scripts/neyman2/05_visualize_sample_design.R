# ==============================================================================
# 05_visualize_sample_design.R
# Purpose: Diagnostic visualization for ALL YEARS (2010, 2016, 2020).
# ==============================================================================

source("scripts/neyman2/00_config.r")

SAMPLE_DIR <- file.path(DERIVED_DIR, "spatial_samples")
OUTPUT_DIR <- file.path(DERIVED_DIR, "diagnostics")
TARGET_YEARS <- c(2010, 2016, 2020)

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

if (!is.null(TARGET_MLRA_IDS)) {
  mlra_ids <- TARGET_MLRA_IDS
} else {
  mlra_ids <- ALL_MLRA_IDS
}

message(paste("Generating Diagnostics for:", paste(mlra_ids, collapse = ", ")))

for (m_id in mlra_ids) {
  message(paste0("\n=== Visualizing MLRA: ", m_id, " ==="))

  # A. Load Shared Background Geometry (Once per MLRA)
  grid_file <- STATIC_INPUTS$grid_1km
  message("   Loading background geometry...")
  grid_sf <- terra::vect(grid_file)
  grid_sf <- grid_sf[grid_sf$MLRA_ID == m_id, ]
  grid_sf <- sf::st_as_sf(grid_sf)
  grid_sf$id <- as.character(grid_sf$id) # Type Safety

  # B. Loop Years
  for (yr in TARGET_YEARS) {
    message(paste0("   --- Year: ", yr, " ---"))

    # Define Year-Specific Paths
    sample_gpkg <- file.path(
      SAMPLE_DIR,
      paste0("MLRA_", m_id, "_GRTS_sample_", yr, ".gpkg")
    )
    master_csv <- file.path(
      DERIVED_DIR,
      "dynamic_attributes",
      paste0("MLRA_", m_id, "_master_dataset.csv")
    )
    alloc_csv <- file.path(
      DERIVED_DIR,
      "neyman_allocations",
      paste0("MLRA_", m_id, "_optimal_allocations.csv")
    )

    if (!file.exists(sample_gpkg)) {
      message(paste("      Sample file missing for", yr, "- skipping."))
      next
    }

    # 1. Load Data
    sites_sf <- sf::st_read(sample_gpkg, quiet = TRUE) %>%
      dplyr::mutate(id = as.character(id))

    pop_df <- readr::read_csv(master_csv, show_col_types = FALSE) %>%
      dplyr::filter(year == yr) %>%
      dplyr::mutate(id = as.character(id))

    alloc_df <- readr::read_csv(alloc_csv, show_col_types = FALSE) %>%
      dplyr::filter(year == yr) %>%
      dplyr::select(id, strata, optimal_variable) %>%
      dplyr::mutate(id = as.character(id))

    # Join
    pop_df <- pop_df %>%
      dplyr::inner_join(alloc_df, by = "id") %>%
      dplyr::mutate(strata_id = paste0("Stratum_", strata))

    # Transform Grid if needed to match sites
    if (sf::st_crs(sites_sf) != sf::st_crs(grid_sf)) {
      grid_sf <- sf::st_transform(grid_sf, sf::st_crs(sites_sf))
    }

    map_sf <- grid_sf %>% dplyr::inner_join(pop_df, by = "id")
    opt_var <- unique(pop_df$optimal_variable)[1]

    # 2. Map Plot
    p_map <- ggplot() +
      geom_sf(data = map_sf, aes(fill = strata_id), color = NA, alpha = 0.5) +
      geom_sf(
        data = sites_sf,
        aes(shape = sample_type, color = sample_type),
        size = 2
      ) +
      scale_fill_viridis_d(option = "mako", name = "Strata") +
      scale_color_manual(
        values = c("Primary" = "red", "Oversample" = "black")
      ) +
      scale_shape_manual(values = c("Primary" = 19, "Oversample" = 1)) +
      labs(
        title = paste0("Sample Design: MLRA ", m_id, " (", yr, ")"),
        subtitle = paste0("Stratification: ", opt_var),
        caption = "Red=Primary, Black=Oversample"
      ) +
      theme_minimal() +
      theme(axis.text = element_blank(), panel.grid = element_blank())

    ggsave(
      file.path(OUTPUT_DIR, paste0("MLRA_", m_id, "_map_", yr, ".png")),
      p_map,
      width = 8,
      height = 8,
      bg = "white"
    )

    # 3. Boxplot
    plot_pop <- pop_df %>% dplyr::select(strata_id, TOF)
    plot_sample <- sites_sf %>%
      sf::st_drop_geometry() %>%
      dplyr::select(id, sample_type) %>%
      dplyr::inner_join(pop_df %>% dplyr::select(id, TOF, strata_id), by = "id")

    p_box <- ggplot() +
      geom_boxplot(
        data = plot_pop,
        aes(x = strata_id, y = TOF),
        outlier.shape = NA,
        fill = "gray95"
      ) +
      geom_jitter(
        data = plot_sample,
        aes(x = strata_id, y = TOF, color = sample_type),
        width = 0.2,
        size = 2,
        alpha = 0.8
      ) +
      scale_color_manual(
        values = c("Primary" = "red", "Oversample" = "black")
      ) +
      labs(title = paste0("Coverage: MLRA ", m_id, " (", yr, ")"), y = "TOF") +
      theme_bw()

    ggsave(
      file.path(OUTPUT_DIR, paste0("MLRA_", m_id, "_boxplot_", yr, ".png")),
      p_box,
      width = 8,
      height = 6
    )

    message(paste0("      Saved plots for ", yr))
  }
}

message("\nVisualization complete.")
