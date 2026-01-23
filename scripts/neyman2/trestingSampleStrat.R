# ==============================================================================
# 04_sampling_visualization_demo_v3.R
# Purpose: Visual comparison of Stratified Systematic vs. Stratified GRTS.
#          (Dependency Free Version)
# ==============================================================================

if (!require("pacman")) {
  install.packages("pacman")
}
# Removed 'patchwork' from the list
pacman::p_load("spsurvey", "sf", "ggplot2", "dplyr")

# --- 1. GENERATE SYNTHETIC LANDSCAPE WITH STRATA ------------------------------
message("Generating synthetic 50x50 grid with 3 Strata...")

grid_dim <- 50
polys <- sf::st_make_grid(
  sf::st_as_sfc(sf::st_bbox(c(
    xmin = 0,
    xmax = grid_dim,
    ymin = 0,
    ymax = grid_dim
  ))),
  n = c(grid_dim, grid_dim)
) %>%
  sf::st_as_sf()

sf::st_crs(polys) <- "EPSG:3857" # Dummy CRS
polys$id <- 1:nrow(polys)

# Create 3 Strata based on Y-coordinates
coords <- sf::st_coordinates(sf::st_centroid(polys))
polys$strata <- case_when(
  coords[, 2] > 35 ~ "1_Top",
  coords[, 2] > 15 ~ "2_Middle",
  TRUE ~ "3_Bottom"
)

# Define Sample Sizes per Stratum
allocations <- c("1_Top" = 10, "2_Middle" = 20, "3_Bottom" = 30)


# --- 2. METHOD A: STRATIFIED SYSTEMATIC (MODULO) ------------------------------
message("Drawing Stratified Systematic Sample...")

sys_points_list <- list()

for (strat in names(allocations)) {
  strat_data <- polys %>% filter(strata == strat)
  n_target <- allocations[[strat]]

  interval <- floor(nrow(strat_data) / n_target)

  set.seed(123)
  start_seed <- sample(1:interval, 1)

  indices <- which((1:nrow(strat_data) - start_seed) %% interval == 0)
  sys_points_list[[strat]] <- strat_data[indices, ] %>% sf::st_centroid()
}

sys_points <- do.call(rbind, sys_points_list)

plot_sys <- ggplot() +
  geom_sf(data = polys, aes(fill = strata), color = NA, alpha = 0.3) +
  geom_sf(data = sys_points, color = "black", size = 2) +
  scale_fill_manual(values = c("gray90", "gray70", "gray50")) +
  ggtitle(
    "Stratified Systematic",
    subtitle = "3 Grids of different densities"
  ) +
  theme_void() +
  theme(plot.title = element_text(face = "bold"), legend.position = "none")


# --- 3. METHOD B: STRATIFIED SPATIALLY BALANCED (GRTS) ------------------------
message("Drawing Stratified GRTS Sample...")

set.seed(123)
grts_result <- spsurvey::grts(
  sframe = polys,
  n_base = allocations,
  stratum_var = "strata"
)
grts_points <- grts_result$sites_base

plot_grts <- ggplot() +
  geom_sf(data = polys, aes(fill = strata), color = NA, alpha = 0.3) +
  geom_sf(data = grts_points, color = "black", size = 2) +
  scale_fill_manual(values = c("gray90", "gray70", "gray50")) +
  ggtitle(
    "Stratified GRTS",
    subtitle = "Balanced density without grid artifacts"
  ) +
  theme_void() +
  theme(plot.title = element_text(face = "bold"), legend.position = "none")


# --- 4. DISPLAY ---------------------------------------------------------------
message("Plotting 1 of 2: Systematic...")
print(plot_sys)

message("Plotting 2 of 2: GRTS... (Check Plots tab)")
print(plot_grts)
