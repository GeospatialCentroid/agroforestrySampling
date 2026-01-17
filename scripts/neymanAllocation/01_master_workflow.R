pacman::p_load(targets, terra, dplyr, readr, purrr, tidyr, furrr)

# Source your functions
walk(list.files("functions/neymanFunctions", full.names = TRUE), source)

# --- CONFIG ---
MLRA_LIST <-  c(79) #81, 86,  87,  88,  89,  90, 142, 146, 150) #  63  72  77  78  79  80  81  86  87  88  89  90 142 146 150
YEAR_LIST <- c("2010", "2016", "2020")
STRAT_VAR <- "Shrubland"
STATE <- "NE"

# Initialize Parallel Backend
# Using 2 cores less than max to prevent system lockup
plan(multisession, workers = 12)

# --- EXECUTION ---
countiesExport <- paste0("data/derived/vector/", STATE, "_counties.gpkg")

if(!file.exists(countiesExport)){
  counties <- tigris::counties(state = STATE) 
  counties <- counties |>
    terra::vect()|>
    terra::project("EPSG:4326")
  terra::writeVector(x = counties, filename = countiesExport)
} else {
  counties <- terra::vect(countiesExport)
}

# Wrap counties so it can be sent to workers safely
counties_packed <- terra::wrap(counties)

# ---------------------------------------------------------
# 1. Process Static Layers
# ---------------------------------------------------------
cat("Processing static layers...\n")

static_results <- MLRA_LIST |> 
  set_names() |> 
  future_map(~{
    # A. Unwrap counties inside the worker
    counties_worker <- terra::unwrap(counties_packed)
    
    # B. Run your existing function
    res <- process_static_mlra(
      mlra_id = .x, 
      path_mlra = "data/derived/mlra/Nebraska_MLRA.gpkg",
      path_grids = "data/derived/grids/Nebraska_1km_mlra.gpkg",
      path_riparian = "~/trueNAS/work/Agroforestry/data/products/riparian/nebraskaRiparian10.tif",
      dir_vect = "data/derived/vector",
      dir_tab = "data/derived/tables",
      state_abbr = "NE",
      counties = counties_worker
    )
    
    # C. WRAP OUTPUTS: Prepare terra objects for transport back to main session
    # We iterate over the result list and wrap any terra objects found
    res_wrapped <- map(res, function(x) {
      if(inherits(x, c("SpatRaster", "SpatVector"))) {
        return(terra::wrap(x))
      } else {
        return(x)
      }
    })
    
    return(res_wrapped)
  }, 
  .options = furrr_options(seed = TRUE), # Fixes "UNRELIABLE VALUE" warning
  .progress = TRUE)


# ---------------------------------------------------------
# 2. Process Dynamic Years & Run Sampling
# ---------------------------------------------------------
cat("Processing dynamic years and sampling...\n")

# Use expand_grid to fix cross_df deprecation warning
work_queue <- tidyr::expand_grid(mlra = MLRA_LIST, year = YEAR_LIST)

final_output <- work_queue |>
  mutate(
    # A. Generate the Clean Dataframe for this Combo
    data_clean = future_map2(mlra, year, function(m, y) {
      
      # Retrieve the WRAPPED static data
      s_data_wrapped <- static_results[[as.character(m)]]
      
      # D. UNWRAP INPUTS: Revive the terra objects inside this new worker
      s_data_unwrapped <- map(s_data_wrapped, function(x) {
        if(inherits(x, c("PackedSpatRaster", "PackedSpatVector"))) {
          return(terra::unwrap(x))
        } else {
          return(x)
        }
      })
      
      # Now run the function with "live" objects
      process_dynamic_year(
        mlra_id = m, 
        year = y, 
        static_data = s_data_unwrapped, 
        path_g12 = "data/raw/grid12M/twelve_mi_grid_uid.gpkg",
        path_tof_dir = "~/trueNAS/work/Agroforestry/data/products/tenMeterModels",
        dir_rast = "data/derived/raster",
        dir_tab = "data/derived/tables"
      )
    }, .options = furrr_options(seed = TRUE)),
    
    # B. Run Sampling Analysis
    sampling_results = future_pmap(list(data_clean, mlra, year), function(df, m, y) {
      run_neyman_pipeline(
        clean_df = df, 
        mlra_id = m, 
        year = y, 
        strat_var = STRAT_VAR,
        dir_res = "data/derived/results",
        dir_plot = "data/derived/plots"
      )
    }, .options = furrr_options(seed = TRUE))
  )

# View final structure
print(final_output)

# Save
saveRDS(final_output, file = paste0("data/derived/results/final_output_master_", STRAT_VAR, ".rds"))