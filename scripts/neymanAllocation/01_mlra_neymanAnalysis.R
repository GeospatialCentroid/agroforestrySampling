# 01_mlra_analysis.R

# for the final table- the random results are not being report in the same fashion as the group methods 
# develop a means of iteratoring over the mlra ids, year, classes 
# 1. Setup & Config -------------------------------------------------------
pacman::p_load(targets, terra, dplyr, readr, tigris, classInt, forcats, purrr, tidyr, ggplot2)

# Source functions
source("functions/neymanFunctions/fct_processing.R")
source("functions/neymanFunctions/fct_sampling.R")
source("functions/neymanFunctions/fct_visualization.R")

# --- MASTER CONFIGURATION ---
MLRA_TARGET <- 150             # The region ID - issue with 63, 72 extent not overlapping 
YEAR_TARGET <- "2020"         # The year for NLCD/Roads
STRAT_VAR   <- "Forest"       # The NLCD Class to stratify by (Must match names created in Step 4)

# --- DIRECTORY ARCHITECTURE ---
# Base Derived Path
DIR_DERIVED <- "data/derived"

# Sub-directories for organized outputs
DIR_VECT <- file.path(DIR_DERIVED, "vector")  # For Roads, Grids, Vectors
DIR_RAST <- file.path(DIR_DERIVED, "raster")  # For TOF Mosaics
DIR_TAB  <- file.path(DIR_DERIVED, "tables")  # For intermediate stats (Road lengths, Area calcs)
DIR_RES  <- file.path(DIR_DERIVED, "results") # For Final Sampling Analysis
DIR_PLOT <- file.path(DIR_DERIVED, "plots")   # For Visualizations

# Create all directories recursively
purrr::walk(c(DIR_VECT, DIR_RAST, DIR_TAB, DIR_RES, DIR_PLOT), 
            ~dir.create(.x, showWarnings = FALSE, recursive = TRUE))

# --- INPUT PATHS ---
PATH_GRIDS_1K <- "data/derived/grids/Nebraska_1km_mlra.gpkg"
PATH_MLRA     <- "data/derived/mlra/Nebraska_MLRA.gpkg"
PATH_RIPARIAN <- "~/trueNAS/work/Agroforestry/data/products/riparian/nebraskaRiparian10.tif"
PATH_GRID12   <- "data/raw/grid12M/twelve_mi_grid_uid.gpkg"
PATH_TOF_DIR  <- "~/trueNAS/work/Agroforestry/data/products/tenMeterModels"

# 2. Load Base Data -------------------------------------------------------
# (Assuming targets or local loads work as before)
tofByMLRA <- tryCatch(tar_read(name = "tof_by_mlra"), error = function(e) NULL) 
nlcdYear    <- tryCatch(tar_read_raw(paste0("nlcd_",YEAR_TARGET)), error = function(e) NULL)

grids1k <- terra::vect(PATH_GRIDS_1K)
mlra    <- terra::vect(PATH_MLRA)
rip     <- terra::rast(PATH_RIPARIAN)
g12     <- terra::vect(PATH_GRID12)
tof10m  <- list.files(PATH_TOF_DIR, full.names = TRUE, recursive = TRUE)

# Subset to AOI
m1 <- mlra[mlra$MLRA_ID == MLRA_TARGET, ]
k1 <- grids1k[grids1k$MLRA_ID == MLRA_TARGET, ]

# Handle NLCD/Riparian cropping (Check if loaded from targets or needs explicit load)
if(!is.null(nlcdYear)) nlcd1 <- nlcdYear |> terra::crop(m1)
riparianData <- rip |> terra::crop(m1)

# 3. Processing Stages ----------------------------------------------------

# # --- A. Roads ---
# roadExport <- file.path(DIR_VECT, paste0("mlra", MLRA_TARGET, "_Roads.gpkg"))
# 
# if (!file.exists(roadExport)) {
#   
#   message("Processing Roads for MLRA ", MLRA_TARGET, "...")
#   options(timeout = 600) # Increase timeout for large downloads
#   
#   # 1. Get Counties for the State
#   # We use the generic state "NE" (Nebraska). Update if your MLRA crosses state lines.
#   ne_counties <- tigris::counties(state = "NE", year = as.numeric(YEAR_TARGET), 
#                                   cb = FALSE, progress_bar = FALSE) |>
#     terra::vect()
#   
#   # 2. Filter to Counties inside the MLRA
#   # We only want to download roads for counties that actually touch our study area
#   target_counties <- ne_counties[m1, ] # Spatial subset
#   county_codes <- target_counties$COUNTYFP
#   
#   message("  Found ", length(county_codes), " overlapping counties.")
#   
#   # 3. Function to Download & Clip Roads
#   get_county_roads <- function(code, mlra_shape) {
#     tryCatch({
#       # Download TIGER roads for specific county
#       r <- tigris::roads(state = "NE", county = code, year = as.numeric(YEAR_TARGET), progress_bar = FALSE)
#       
#       # Convert sf -> terra
#       v <- terra::vect(r)
#       
#       # Transform to match MLRA projection (vital before clipping)
#       v <- terra::project(v, terra::crs(mlra_shape))
#       
#       # Clip to MLRA immediately to save RAM
#       v_clipped <- terra::crop(v, mlra_shape)
#       
#       return(v_clipped)
#     }, error = function(e) {
#       warning("  Failed to download roads for county: ", code)
#       return(NULL)
#     })
#   }
#   
#   # 4. Iterate and Combine
#   # using map to loop through codes
#   all_roads_list <- purrr::map(
#     .x = county_codes, 
#     .f = get_county_roads, 
#     mlra_shape = m1
#   )
#   
#   # Remove failures (NULLs) and bind into one object
#   roads <- all_roads_list |> 
#     purrr::discard(is.null) |> 
#     terra::vect()
#   
#   # 5. Save
#   terra::writeVector(x = roads, filename = roadExport, overwrite = TRUE)
#   message("  Roads saved to: ", roadExport)
#   
# } else {
#   message("Loading existing roads from: ", roadExport)
#   roads <- terra::vect(roadExport)
# }
# 
# # --- B. Road Lengths ---
# roadLengthExport <- file.path(DIR_TAB, paste0("roadLength_mlra", MLRA_TARGET, ".csv"))
# 
# if (!file.exists(roadLengthExport)) {
#   roadLength <- purrr::map(
#     .x = k1$id,
#     .f = processRoadLength,
#     grids1k = k1,
#     roads = roads
#   ) |> dplyr::bind_rows()
#   readr::write_csv(roadLength, file = roadLengthExport)
# } else {
#   roadLength <- readr::read_csv(roadLengthExport)
# }

# --- C. Riparian Area ---
ripAreaExport <- file.path(DIR_TAB, paste0("riparianArea_mlra", MLRA_TARGET, ".csv"))

if (!file.exists(ripAreaExport)) {
  out1 <- purrr::map(
    .x = k1$id,
    .f = processRipArea,
    grids1k = k1,
    riparianData = riparianData
  ) |> dplyr::bind_rows()
  readr::write_csv(out1, file = ripAreaExport)
} else {
  out1 <- readr::read_csv(ripAreaExport)
}

# --- D. NLCD Area ---
nlcdAreaExport <- file.path(DIR_TAB, paste0("nlcdArea_mlra", MLRA_TARGET, "_", YEAR_TARGET, ".csv"))

if (!file.exists(nlcdAreaExport)) {
  out2 <- purrr::map(
    .x = k1$id,
    .f = processNLCDArea,
    grids1k = k1,
    nlcdData = nlcd1,
    year = YEAR_TARGET
  ) |> dplyr::bind_rows()
  readr::write_csv(out2, file = nlcdAreaExport)
} else {
  out2 <- readr::read_csv(nlcdAreaExport)
}

# --- E. TOF Mosaicing & Area ---
exportTOF <- file.path(DIR_RAST, paste0("mlra", MLRA_TARGET, "_TOF_", YEAR_TARGET, ".tif"))

if (!file.exists(exportTOF)) {
  # Assuming gatherAllTOF handles the filtering by MLRA geometry
  rasters <- gatherAllTOF(mlra = m1, g12 = g12, sel10m = tof10m, year = YEAR_TARGET) |>
    terra::mosaic(fun = "max")
  terra::writeRaster(x = rasters, filename = exportTOF)
} else {
  rasters <- terra::rast(exportTOF)
}

tofAreaExport <- file.path(DIR_TAB, paste0("tofArea_mlra", MLRA_TARGET,"_",YEAR_TARGET, ".csv"))

if (!file.exists(tofAreaExport)) {
  tofAreas <- purrr::map(
    .x = k1$id,
    .f = processTOF_areas,
    grids1k = k1,
    cotData = rasters
  ) |> dplyr::bind_rows()
  readr::write_csv(tofAreas, file = tofAreaExport)
} else {
  tofAreas <- readr::read_csv(tofAreaExport)
}

# 4. Feature Engineering --------------------------------------------------

# Helper function
sum_cols <- function(...) rowSums(dplyr::pick(...), na.rm = TRUE)

# 1. Aggregate NLCD (Creates columns: Forest, Water, Shrubland, etc.)
nlcd8 <- out2 |>
  dplyr::mutate(
    Water = sum_cols(starts_with("class_1")),
    Developed = sum_cols(starts_with("class_2")),
    Barren = sum_cols(starts_with("class_3")),
    Forest = sum_cols(starts_with("class_4")),
    Shrubland = sum_cols(starts_with("class_5")),
    Herbaceous = sum_cols(starts_with("class_7")),
    Planted = sum_cols(starts_with("class_8")),
    Wetlands = sum_cols(starts_with("class_9"))
  ) |>
  dplyr::select(-starts_with("class_"))

# 2. Calculate Z-Scores
ripZ <- out1 |>
  dplyr::select(gridID, percentRiparian) |>
  addZscoreMetrics("percentRiparian")

tofZ <- tofAreas |>
  dplyr::select(gridID, gridArea = totalArea, percentTOF) |>
  addZscoreMetrics("percentTOF")

# roadZ <- roadLength |>
#   addZscoreMetrics("roadLengthKM") # Assuming column name in roadLength df

# 3. Merge All Data
joinData <- tofZ |>
  dplyr::left_join(ripZ, by = "gridID") |>
  dplyr::left_join(nlcd8, by = "gridID") |>
  # dplyr::left_join(roadZ, by = "gridID") |>
  dplyr::select(gridID, gridArea, year, everything())

# Save Intermediate Table
readr::write_csv(joinData, file.path(DIR_TAB, paste0("allZScore_mlra", MLRA_TARGET,"_",YEAR_TARGET, ".csv")))

# 5. Correlation & Sampling Prep (DYNAMIC) --------------------------------

# Select the Dynamic Stratification Variable
# We rename the chosen config var (e.g., 'Forest') to 'strat_var'
# so the rest of the code becomes generic.
df_sampling <- joinData |>
  dplyr::select(gridID, percentTOF, strat_var = dplyr::all_of(STRAT_VAR)) |>
  dplyr::filter(is.finite(strat_var)) |>
  dplyr::mutate(
    # 1. Equal Interval
    group_equal = cut(strat_var, breaks = 3, include.lowest = TRUE, labels = FALSE),
    
    # 2. Standard Quantile
    group_quantile = cut(strat_var, 
                         breaks = unique(quantile(strat_var, probs = seq(0, 1, length.out = 4))), 
                         include.lowest = TRUE, labels = FALSE),
    
    # 3. Custom Zero-Inflated Quantile (Handles whatever class you picked)
    group_customQuantile = dplyr::case_when(
      strat_var == 0 ~ 1,
      strat_var <= median(strat_var[strat_var > 0]) ~ 2,
      TRUE ~ 3
    ),
    
    # 4. K-Means
    group_kmeans = {
      # Handle case if variance is 0 (all same values)
      if(var(strat_var) == 0) {
        rep(1, n()) 
      } else {
        km <- kmeans(strat_var, centers = 3)
        as.integer(forcats::fct_reorder(as.factor(km$cluster), strat_var, .fun = mean))
      }
    },
    
    # 5. SRS
    group_srs = 1
  )

# 6. Neyman Allocation & Simulation ---------------------------------------

TOTAL_N <- 100

# Allocations (Using the generic 'strat_var' and dynamic groups)
alloc_kmeans <- get_neyman_allocation(df_sampling, group_kmeans, percentTOF, TOTAL_N, splitType = "kmeans")
alloc_equal  <- get_neyman_allocation(df_sampling, group_equal, percentTOF, TOTAL_N, splitType = "equalInterval")
alloc_quantile <- get_neyman_allocation(df_sampling, group_quantile, percentTOF, TOTAL_N, splitType = "quantile")
alloc_zeroQ  <- get_neyman_allocation(df_sampling, group_customQuantile, percentTOF, TOTAL_N, splitType = "zero_Quantile")
alloc_srs    <- get_neyman_allocation(df_sampling, group_srs, percentTOF, TOTAL_N, splitType = "SRS")

data_list <- list(
  equal = list(data = alloc_equal, name = "group_equal"),
  kmeans = list(data = alloc_kmeans, name = "group_kmeans"),
  quantile = list(data = alloc_quantile, name = "group_quantile"),
  zeroQuantile = list(data = alloc_zeroQ, name = "group_customQuantile"),
  srs = list(data = alloc_srs, name = "group_srs")
)

# Simulation Setup
removedGrids <- joinData$gridID[is.na(joinData[[STRAT_VAR]])] 
k2 <- k1[!k1$id %in% removedGrids, ]
k3 <- data.frame(id = k2$id, areas = terra::expanse(k2, unit = "km"))

df_t <- df_sampling |> dplyr::left_join(k3, by = c("gridID" = "id"))

# Calculate Truth
totalArea <- sum(k3$areas, na.rm = TRUE)
fullRatio <- weighted.mean(x = df_t$percentTOF, w = df_t$areas, na.rm = TRUE)
tofMeasured <- (fullRatio / 100) * totalArea
low_bound <- tofMeasured - (tofMeasured * 0.1) # 10% Bounds
high_bound <- tofMeasured + (tofMeasured * 0.1)

# Run Simulation
final_results <- purrr::map_dfr(
  .x = data_list,
  .f = runSampling_neyman,
  tofDF = df_t,
  totalArea = totalArea,
  runIterations = 100,
  low = low_bound,
  high = high_bound,
  true_value = tofMeasured
)

# Export Raw Results
write_csv(final_results, file.path(DIR_RES, paste0("neymanResults_MLRA", MLRA_TARGET, "_",YEAR_TARGET,".csv")))

# 7. Visualization & Thresholds -------------------------------------------

# 1. Convergence Curve
p1 <- plot_convergence_curve(final_results)
plot_name_1 <- paste0("convergence_curve_MLRA", MLRA_TARGET,"_",YEAR_TARGET, ".png")
ggsave(file.path(DIR_PLOT, plot_name_1), plot = p1, width = 10, height = 6)

# 2. Efficiency Snapshot
p2 <- plot_efficiency_bar(final_results, target_runs = c(5, 10, 20))
plot_name_2 <- paste0("efficiency_snapshot_MLRA", MLRA_TARGET,"_",YEAR_TARGET, ".png")
ggsave(file.path(DIR_PLOT, plot_name_2), plot = p2, width = 10, height = 6)

# 3. Viability Table & Plot
viability_table <- get_viability_thresholds(
  final_results,
  total_population = nrow(df_t),
  acc_threshold = 90,
  cov_threshold = 90
)

print(viability_table)

p3 <- plot_efficiency_thresholds(final_results, viability_table)
plot_name_3 <- paste0("efficiency_thresholds_MLRA", MLRA_TARGET,"_",YEAR_TARGET, ".png")
ggsave(file.path(DIR_PLOT, plot_name_3), plot = p3, width = 10, height = 6)


# 8. Validation: Systematic vs Random -------------------------------------

# Identify the Winner (Rank 1)
winner_row <- viability_table %>% 
  dplyr::filter(method != "group_srs") %>% 
  dplyr::arrange(pct_of_population) %>% 
  dplyr::slice(1)

# Setup Target for Validation (Dynamically select the generic 'strat_var' column mapped to 'group')
target_method_name <- winner_row$method
target_alloc <- data_list[[which(sapply(data_list, function(x) x$name == target_method_name))]]$data

target_df <- df_t %>%
  dplyr::select(gridID, percentTOF, group = dplyr::all_of(target_method_name), areas)

# Validation Loop (SRS Mode)
runMax <- floor(nrow(target_df) / 100)
random_results <- data.frame()

for (i in 1:runMax) {
  sim_out <- run_random_validation(
    data = target_df,
    allocation_df = target_alloc,
    total_area = totalArea,
    iterations = 100, 
    seed = 123 + i, 
    nth = i,
    true_value = tofMeasured
  )
  summary_row <- data.frame(
    runNumber = i,
    percentTRUE = (sum(sim_out$tofEstimate >= low_bound & sim_out$tofEstimate <= high_bound) / 100) * 100,
    percentCOVERED = (sum(sim_out$isCovered, na.rm = TRUE) / 100) * 100,
    method = paste0(target_method_name, "_RANDOM") 
  )
  random_results <- rbind(random_results, summary_row)
}

# Validation Plot
comparison_df <- bind_rows(
  final_results %>% filter(method == target_method_name),
  random_results
)
p4 <- plot_convergence_curve(comparison_df) +
  ggplot2::labs(
    title = "Validation: Systematic vs. Random Sampling",
    subtitle = "Overlap indicates systematic design introduces no spatial bias"
  )
plot_name_4 <- paste0("validation_random_vs_systematic_MLRA", MLRA_TARGET, "_",YEAR_TARGET,".png")
ggsave(file.path(DIR_PLOT, plot_name_4), plot = p4, width = 10, height = 6)


# 9. Final Executive Summary & Export -------------------------------------

# Combine for Master Table
validation_row <- random_results %>%
  dplyr::filter(runNumber == winner_row$run_number) %>%
  dplyr::mutate(
    Type = "Validation (Random Mode)",
    pct_of_population = winner_row$pct_of_population,
    raw_samples = winner_row$raw_samples,
    accuracy_at_min = percentTRUE,
    coverage_at_min = percentCOVERED
  ) %>%
  dplyr::select(Type, method, pct_of_population, raw_samples, accuracy_at_min, coverage_at_min)

final_summary_table <- dplyr::bind_rows(
  viability_table %>%
    dplyr::filter(method != "group_srs") %>%
    dplyr::arrange(pct_of_population) %>%
    dplyr::slice(1:2) %>%
    dplyr::mutate(Type = c("Top Performer (Winner)", "2nd Most Efficient")),
  validation_row,
  viability_table %>%
    dplyr::filter(method == "group_srs") %>%
    dplyr::mutate(Type = "Baseline (Census/SRS)")
) %>%
  dplyr::mutate(
    MLRA = MLRA_TARGET,
    Year = YEAR_TARGET,
    Stratification_Class = STRAT_VAR
  ) %>%
  dplyr::select(MLRA, Year, Stratification_Class, Type, Method = method, `Sample Size (%)` = pct_of_population, everything())

# Dynamic Filename
file_name <- paste0("sampling_summary_MLRA", MLRA_TARGET, "_", YEAR_TARGET, "_", STRAT_VAR, ".csv")
write_csv(final_summary_table, file.path(DIR_RES, file_name))

print(final_summary_table)
message("Analysis Complete. Summary saved to: ", file.path(DIR_RES, file_name))
