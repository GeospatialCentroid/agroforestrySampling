# ==============================================================================
# 00_run_pipeline.R
# Purpose: Orchestrates the end-to-end data processing pipeline for Neyman Allocation.
# Usage:   Run this script to execute the entire workflow.
#          Ensure 'TARGET_MLRA_IDS' is set correctly in '00_config.r' before running.
# ==============================================================================

# Load tictoc explicitly for the master timer (if not yet loaded)
pacman::p_load(tictoc)
# Start global timer
tictoc::tic("Full Pipeline Execution")

# --- STEP 1: CONFIGURATION ----------------------------------------------------
# Purpose: Load required libraries, define global file paths, set CRS constants,
#          and define the execution scope (which MLRAs to process).
message("\n>>> STEP 1: LOADING CONFIGURATION")
source("scripts/neyman2/00_config.r")


# --- STEP 2: STATIC PROCESSING ------------------------------------------------
# Purpose: Process variables that do not change based on dynamic reference grids.
#          1. Iterates through target MLRAs.
#          2. Attributes 1km grids with Riparian percentages.
#          3. Attributes 1km grids with NLCD Land Cover percentages (2010/16/20).
#          4. Aggregates raw NLCD codes into 8 primary classes (Forest, Water, etc.).
message("\n>>> STEP 2: RUNNING STATIC PROCESSING")
source("scripts/neyman2/01_static_processing.R")


# --- STEP 3: DYNAMIC PROCESSING -----------------------------------------------
# Purpose: Process Time of Flight (TOF) and merge into the final Master Dataset.
#          1. Maps 1km grids to their parent 12-mile COT raster tiles.
#          2. Builds Virtual Rasters (VRTs) for efficient memory usage.
#          3. Extracts pixel counts (0-9) to calculate TOF % for 2010/16/20.
#          4. Merges Static and Dynamic data.
#          5. Pivots data to Long Format (Year as a variable) for analysis.
message("\n>>> STEP 3: RUNNING DYNAMIC PROCESSING")
source("scripts/neyman2/02_dynamic_processing.R")


# --- COMPLETION ---------------------------------------------------------------
message(
  "\n=============================================================================="
)
message("PIPELINE COMPLETE")
message("Outputs located in: ", DERIVED_DIR)
message("Master Datasets ready for Neyman Allocation.")
message(
  "=============================================================================="
)

# Stop global timer
tictoc::toc()
