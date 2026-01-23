# ==============================================================================
# 00_config.R
# Purpose: Centralized library loading and file path definitions for workflow.
# Usage: Source this file at the start of every script to ensure consistency.
# ==============================================================================

##
# --- 1. LIBRARY MANAGEMENT ----------------------------------------------------
# Load necessary packages using pacman for efficient installation and loading
pacman::p_load(
    "terra",
    "sf",
    "dplyr",
    "purrr",
    "tidyr",
    "readr",
    "stringr",
    "ggplot2",
    "tictoc",
    "exactextractr"
)

# --- 2. GLOBAL SETTINGS -------------------------------------------------------
## Coordinate Reference Systems (CRS)
# Default CRS for US-wide analysis
ALBERS_CRS <- "EPSG:5070" # NAD83 / Conus Albers
# Common global coordinate system
WGS_CRS <- "EPSG:4326" # WGS 84

## MLRA Processing Configuration
# Specify target MLRA IDs for processing (set to NULL to process all)
# Currently set to process MLRAs: 78, 86, 150
TARGET_MLRA_IDS <- c("78", "86", "150")
ALL_MLRA_IDS <- c(
    "63",
    "72",
    "77",
    "78",
    "79",
    "80",
    "81",
    "86",
    "87",
    "88",
    "89",
    "90",
    "142",
    "146",
    "150"
)

# --- 3. INPUT PATHS ------------------------------------------------------------

## Base Directories
INPUT_DIR <- "data/raw" # Path to raw input data
DERIVED_DIR <- "data/derived" # Directory for derived outputs
PRODUCTS_DIR <- "data/products" # Final product storage location

## Static Input Sources
# Grid Definitions
STATIC_INPUTS <- list(
    # Grids
    grid_12mile = file.path(INPUT_DIR, "grid12M/twelve_mi_grid_uid.gpkg"),
    grid_1km = file.path(DERIVED_DIR, "grids/Nebraska_1km_mlra.gpkg"),

    # MLRA
    mlra = file.path(DERIVED_DIR, "mlra/Nebraska_MLRA.gpkg"),

    # Riparian (Ensure this is not NULL)
    riparian = file.path(INPUT_DIR, "riparianArea/riparianArea10.tif"),

    # NLCD List
    nlcd = list(
        y2010 = file.path(
            INPUT_DIR,
            "nlcd/Annual_NLCD_LndCov_2010_CU_C1V1.tif"
        ),
        y2016 = file.path(
            INPUT_DIR,
            "nlcd/Annual_NLCD_LndCov_2016_CU_C1V1.tif"
        ),
        y2020 = file.path(INPUT_DIR, "nlcd/Annual_NLCD_LndCov_2020_CU_C1V1.tif")
    )
)


# --- VALIDATION CHECK ---------------------------------------------------------
# Ensure inputs are not NULL
if (is.null(STATIC_INPUTS$riparian)) {
    stop("Error: STATIC_INPUTS$riparian is NULL. Check config.")
}
if (is.null(STATIC_INPUTS$nlcd$y2010)) {
    stop("Error: STATIC_INPUTS$nlcd$y2010 is NULL. Check config.")
}

# Ensure files actually exist on disk
required_files <- c(
    STATIC_INPUTS$riparian,
    STATIC_INPUTS$nlcd$y2010,
    STATIC_INPUTS$nlcd$y2016,
    STATIC_INPUTS$nlcd$y2020
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
    stop(paste(
        "The following input files are missing from disk:\n",
        paste(missing_files, collapse = "\n ")
    ))
}


message("Configuration loaded successfully.")
