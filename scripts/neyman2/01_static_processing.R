# ==============================================================================
# 01_static_processing.R
# Purpose: Attribute 1km vector grids with NLCD and Riparian raster data.
#          It handles calculating raw class percentages and aggregating them
#          into primary land cover groups (Forest, Water, etc.).
# Output:  A combined CSV of static attributes for each 1km grid cell per MLRA.
# ==============================================================================

source("scripts/neyman2/00_config.r")

# --- 1. SETUP & LOCAL PATHS ---------------------------------------------------
# Define local output directory for static attributes
OUTPUT_STATIC_DIR <- file.path(DERIVED_DIR, "static_attributes")

if (!dir.exists(OUTPUT_STATIC_DIR)) {
    dir.create(OUTPUT_STATIC_DIR, recursive = TRUE)
}

# --- 2. LOAD INPUTS -----------------------------------------------------------
message("Loading spatial inputs...")

# Load Vector Data
mlra_poly <- terra::vect(STATIC_INPUTS$mlra)
grid_1km <- terra::vect(STATIC_INPUTS$grid_1km)

# Load Raster Data
r_riparian <- terra::rast(STATIC_INPUTS$riparian)

# Load NLCD rasters into a named list for iteration
r_nlcd_list <- list(
    "2010" = terra::rast(STATIC_INPUTS$nlcd$y2010),
    "2016" = terra::rast(STATIC_INPUTS$nlcd$y2016),
    "2020" = terra::rast(STATIC_INPUTS$nlcd$y2020)
)


# --- 3. HELPER FUNCTIONS ------------------------------------------------------

#' Aggregate NLCD Columns into Primary Groups
#'
#' Maps raw NLCD codes (e.g., 41, 42, 43) to parent classes (e.g., "Forest").
#' @param df Dataframe containing columns starting with prefix (e.g., "nlcd_2016_class_11")
#' @param prefix The year prefix used in column naming (e.g., "nlcd_2016")
#' @return Dataframe with added aggregate columns
aggregate_nlcd_classes <- function(df, prefix) {
    # Helper to sum columns safely (returns 0 if no columns match pattern)
    # This handles cases where a specific class (e.g., Wetlands) might be missing entirely from an MLRA
    sum_cols <- function(pattern) {
        # Match columns that start with prefix AND end with the class codes
        cols <- df %>%
            dplyr::select(matches(paste0("^", prefix, "_class_", pattern, "$")))

        if (ncol(cols) == 0) {
            return(0)
        }
        rowSums(cols, na.rm = TRUE)
    }

    # create new aggregated columns
    df %>%
        dplyr::mutate(
            !!paste0(prefix, "_Water") := sum_cols("11|12"), # Open Water, Perennial Ice/Snow
            !!paste0(prefix, "_Developed") := sum_cols("21|22|23|24"), # Developed (Open, Low, Med, High)
            !!paste0(prefix, "_Barren") := sum_cols("31"), # Barren Land
            !!paste0(prefix, "_Forest") := sum_cols("41|42|43"), # Deciduous, Evergreen, Mixed Forest
            !!paste0(prefix, "_Shrubland") := sum_cols("51|52"), # Dwarf Scrub, Shrub/Scrub
            !!paste0(prefix, "_Herbaceous") := sum_cols("71|72|73|74"), # Grassland, Sedge, Lichens, Moss
            !!paste0(prefix, "_Cultivated") := sum_cols("81|82"), # Pasture/Hay, Cultivated Crops
            !!paste0(prefix, "_Wetlands") := sum_cols("90|95") # Woody Wetlands, Emergent Herbaceous Wetlands
        )
}

#' Calculate Class Percentages for Vector Polygons
#'
#' Extracts raster values for each polygon and summarizes them as a percentage of area.
#' @param vect_layer Terra vector polygons (1km grids)
#' @param raster_layer Terra raster object
#' @param prefix String to prefix column names (e.g., "nlcd_2010")
#' @return Dataframe with grid IDs and columns for class percentages
calculate_raster_stats <- function(vect_layer, raster_layer, prefix) {
    # Ensure CRS alignment
    if (terra::crs(raster_layer) != terra::crs(vect_layer)) {
        vect_layer <- terra::project(vect_layer, terra::crs(raster_layer))
    }

    message(paste0("   ...extracting ", prefix))

    # Extract values (returns ID=row_index and Value)
    extract_df <- terra::extract(raster_layer, vect_layer, fun = NULL)

    val_col <- names(extract_df)[2]

    # Summarize to percentages (Wide Format)
    stats_df <- extract_df %>%
        dplyr::group_by(ID, !!rlang::sym(val_col)) %>%
        dplyr::summarise(count = n(), .groups = "drop_last") %>%
        dplyr::mutate(total = sum(count)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(percent = (count / total) * 100) %>%
        dplyr::select(ID, class = !!rlang::sym(val_col), percent) %>%
        tidyr::pivot_wider(
            names_from = class,
            values_from = percent,
            values_fill = 0,
            names_prefix = paste0(prefix, "_class_")
        )

    # Attach back to original IDs using index join (Safer than bind_cols)
    id_map <- data.frame(ID = 1:nrow(vect_layer), id = vect_layer$id)

    result <- id_map %>%
        dplyr::left_join(stats_df, by = "ID") %>%
        dplyr::select(-ID)

    return(result)
}


# --- 4. MAIN PROCESSING LOOP --------------------------------------------------

# 1. Get all available IDs from the polygon file
available_ids <- unique(mlra_poly$MLRA_ID)

# 2. Filter based on Config 'TARGET_MLRA_IDS'
if (!is.null(TARGET_MLRA_IDS)) {
    # Keep only IDs that exist in both the config request and the polygon file
    mlra_ids <- intersect(available_ids, TARGET_MLRA_IDS)

    # Warn if a requested ID was not found
    missing_ids <- setdiff(TARGET_MLRA_IDS, available_ids)
    if (length(missing_ids) > 0) {
        warning(paste(
            "Requested MLRA IDs not found in polygon file:",
            paste(missing_ids, collapse = ", ")
        ))
    }
} else {
    # If NULL, run everything
    mlra_ids <- available_ids
}

# Stop if filter resulted in zero MLRAs
if (length(mlra_ids) == 0) {
    stop("No valid MLRA IDs found to process.")
}

message(paste(
    "Found",
    length(mlra_ids),
    "MLRAs to process:",
    paste(mlra_ids, collapse = ", ")
))

# --- Loop through each MLRA ---
for (m_id in mlra_ids) {
    message(paste0("\nProcessing MLRA: ", m_id))

    out_file <- file.path(
        OUTPUT_STATIC_DIR,
        paste0("MLRA_", m_id, "_static_attributes.csv")
    )

    # Check if output already exists
    if (file.exists(out_file)) {
        message("   Output exists. Skipping.")
        next
    }

    # A. Subset Grids for this MLRA
    mlra_subset <- grid_1km[grid_1km$MLRA_ID == m_id, ]

    if (nrow(mlra_subset) == 0) {
        message("   No grids found in this MLRA. Skipping.")
        next
    }

    message(paste0("   Found ", nrow(mlra_subset), " grid cells."))

    # B. Riparian Extraction
    riparian_stats <- calculate_raster_stats(
        mlra_subset,
        r_riparian,
        "riparian"
    )

    # C. NLCD Extraction (Iterate over years)
    nlcd_results <- list()

    for (year in names(r_nlcd_list)) {
        message(paste0("   Processing NLCD ", year))

        # 1. Get raw class columns (e.g., nlcd_2016_class_41)
        raw_stats <- calculate_raster_stats(
            mlra_subset,
            r_nlcd_list[[year]],
            paste0("nlcd_", year)
        )

        # 2. Add Aggregate columns (e.g., nlcd_2016_Forest)
        agg_stats <- aggregate_nlcd_classes(raw_stats, paste0("nlcd_", year))

        nlcd_results[[year]] <- agg_stats
    }

    # D. Merge All Attributes
    combined_df <- as.data.frame(mlra_subset) %>%
        dplyr::select(id, MLRA_ID) %>%
        dplyr::left_join(riparian_stats, by = "id")

    for (year in names(nlcd_results)) {
        combined_df <- dplyr::left_join(
            combined_df,
            nlcd_results[[year]],
            by = "id"
        )
    }

    # E. Save Output
    readr::write_csv(combined_df, out_file)
    message(paste0("   Saved: ", out_file))
}

message("\nStatic processing complete.")
