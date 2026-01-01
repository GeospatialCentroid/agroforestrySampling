# testing the sampling method before embedding into targets

# focus on single MLRA for 2020
# 80 66 Dakota-Nebraska Eroded Tableland
# get the attibutes to all single 1km grids
pacman::p_load(targets, terra, dplyr, readr, tigris, classInt, forcats, purrr)

# load in some existing targets
tofByMLRA <- tar_read(name = "tof_by_mlra")
nlcdByMLRA <- tar_read(name = "nlcd20_by_mlra")
ripByMLRA <- tar_read(name = "riparian_by_mlra")


# grid12 <- tar_read(name = "grid12")
grids1k <- terra::vect("data/derived/grids/Nebraska_1km_mlra.gpkg")
nlcd20 <- tar_read(nlcd_2020)
mlra <- terra::vect(
  "~/trueNAS/work/agroforestrySampling/data/derived/mlra/Nebraska_MLRA.gpkg"
)
rip <- terra::rast(
  "/home/dune/trueNAS/work/Agroforestry/data/products/riparian/nebraskaRiparian10.tif"
)
# limit data to the Dakota-Nebraska Eroded Tableland
m1 <- mlra[mlra$MLRA_ID == 80, ]

# crop the 1k area to aoi
k1 <- grids1k[grids1k$MLRA_ID == 80, ]

nlcd1 <- nlcd20 |>
  terra::crop(m1)
riparianData <- rip |>
  terra::crop(m1)
# paths to the TOF areas
tof10m <- list.files(
  "~/trueNAS/work/Agroforestry/data/products/tenMeterModels",
  full.names = TRUE,
  recursive = TRUE
)
g12 <- terra::vect(
  "~/trueNAS/work/agroforestrySampling/data/raw/grid12M/twelve_mi_grid_uid.gpkg"
)


# zscore functions  ------------------------------------------------------
addZscoreMetrics <- function(df, target_cols) {
  # Iterate through each column name provided in the vector
  for (col in target_cols) {
    # 1. Check if column exists to prevent errors
    if (!col %in% names(df)) {
      warning(paste("Column", col, "not found in dataframe. Skipping."))
      next
    }
    # 2. Calculate Mean and SD for the current column
    mu <- mean(df[[col]], na.rm = TRUE)
    sigma <- sd(df[[col]], na.rm = TRUE)
    # 3. Calculate the Absolute Z-score
    z_col_name <- paste0(col, "_Zscore")
    # Calculate and assign the absolute z-score to the new column
    df[[z_col_name]] <- abs((df[[col]] - mu) / sigma)

    # 4. Create the Grouping Column
    # We construct a new group name: e.g., "percentRiparian_group"
    group_col_name <- paste0(col, "_group")

    # Apply the logic: 0-1, 1-2, >2
    df[[group_col_name]] <- case_when(
      df[[z_col_name]] <= 1 ~ "0",
      df[[z_col_name]] > 1 & df[[z_col_name]] <= 2 ~ "1",
      df[[z_col_name]] > 2 ~ "2",
      TRUE ~ NA_character_ # Handle missing data
    )
  }

  return(df)
}


# process roads for MLRA  ------------------------------------------------
processRoads <- function(countyIDs, roadFiles, mlra) {
  # select road file of interest
  r1 <- roadFiles[grepl(pattern = paste0("_", countyIDs, "_"), x = roadFiles)]
  if (length(r1) > 0) {
    # read in and crop/mask to mlra
    r2 <- terra::vect(r1) |>
      terra::project(mlra) |>
      terra::intersect(mlra)
    # return
    return(r2)
  } else {
    return(NULL)
  }
}
roadExport <- "temp/mlra80_Roads.gpkg"
if (!file.exists(roadExport)) {
  ne_counties <- counties(state = "NE", year = 2020, cb = TRUE) |>
    terra::vect() |>
    terra::mask(m1)
  countyId <- ne_counties$COUNTYFP

  # generate a series of road files
  roads <- purrr::map(
    .x = countyId,
    .f = processRoads,
    roadFiles = roadFiles,
    mlra = m1
  ) |>
    purrr::discard(is.null)

  # bind to single feature
  roads <- do.call(rbind, roads)
  # export
  terra::writeVector(x = roads, filename = roadExport)
} else {
  roads <- terra::vect(roadExport)
}

processRoadLength <- function(gridID, grids1k, roads) {
  # select grid
  g1 <- grids1k[grids1k$id == gridID, ]
  # process rip to area
  r2 <- roads |>
    terra::intersect(g1) |>
    terra::perim()
  # export results as dataframe
  df <- data.frame(
    gridID = g1$id,
    roadLengthKM = sum(r2, na.rm = TRUE) / 1000
  )
}
# apply the function to
roadLengthExport <- "temp/roadLength_mlra80.csv"
# run the method
if (!file.exists(roadLengthExport)) {
  # a couple of minutes with all 8k features
  roadLength <- purrr::map(
    .x = k1$id,
    .f = processRoadLength,
    grids1k = k1,
    roads = roads
  ) |>
    dplyr::bind_rows()
  write_csv(out1, file = roadLengthExport)
} else {
  roadLength <- readr::read_csv(roadLengthExport)
}


# Process Riparian data layer  -------------------------------------------

# riparian processing - only values on 1 and NA
processRipArea <- function(gridID, grids1k, riparianData) {
  # select grid
  g1 <- grids1k[grids1k$id == gridID, ]
  # process rip to area
  r2 <- riparianData |>
    terra::crop(g1) |>
    terra::mask(g1)
  # generate a cell size raster
  fullArea <- terra::cellSize(x = r2, unit = "km")
  ripArea <- r2 * fullArea
  # calcualte the total area and riparian area
  totArea <- sum(terra::values(fullArea), na.rm = TRUE)
  rArea <- sum(terra::values(ripArea), na.rm = TRUE)

  # export results as dataframe
  df <- data.frame(
    gridID = g1$id,
    riparianArea = rArea,
    totalArea = totArea,
    percentRiparian = (rArea / totArea) * 100
  )
  return(df)
}
# run the method
if (!file.exists("temp/riparianArea_mlra80.csv")) {
  # a couple of minutes with all 8k features
  out1 <- purrr::map(
    .x = k1$id,
    .f = processRipArea,
    grids1k = k1,
    riparianData = riparianData
  ) |>
    dplyr::bind_rows()
  write_csv(out1, file = "temp/riparianArea_mlra80.csv")
} else {
  out1 <- readr::read_csv("temp/riparianArea_mlra80.csv")
}


# process nlcd area ------------------------------------------------------
processNLCDArea <- function(gridID, grids1k, nlcdData, year) {
  # select grid
  g1 <- grids1k[grids1k$id == gridID, ]
  # process rip to area
  r2 <- nlcdData |>
    terra::crop(g1) |>
    terra::mask(g1)
  # testing -- difference betwee 0.148(current), 0.142(original)
  # might be worth evaluating futher
  # r2 <- riparianData |>
  #   terra::crop(g1)|>
  #   terra::crop(g1)

  # generate a cell size raster
  fullArea <- terra::cellSize(x = r2, unit = "km")
  ripArea <- r2 * fullArea
  # calcualte the total area and riparian area
  rastArea <- terra::expanse(x = r2, unit = "km", byValue = TRUE) |>
    dplyr::select(-layer) |>
    tidyr::pivot_wider(
      names_from = value,
      values_from = area,
      names_prefix = "class_"
    ) |>
    dplyr::mutate(
      total_rast_area = rowSums(
        dplyr::across(dplyr::starts_with("class_")),
        na.rm = TRUE
      )
    ) |>
    dplyr::mutate("gridID" = g1$id, year = year) |>
    dplyr::select(gridID, year, everything())
  return(rastArea)
}

# run the method
if (!file.exists("temp/nlcdArea_mlra80.csv")) {
  # a couple of minutes with all 8k features
  out2 <- purrr::map(
    .x = k1$id,
    .f = processNLCDArea,
    grids1k = k1,
    nlcdData = nlcd1,
    year = "2020"
  ) |>
    dplyr::bind_rows()
  write_csv(out2, file = "temp/nlcdArea_mlra80.csv")
} else {
  out2 <- readr::read_csv("temp/nlcdArea_mlra80.csv")
}


# process TOF area  ------------------------------------------------------
gatherAllTOF <- function(mlra, g12, sel10m, year) {
  # select all intersecting grids
  grids <- g12[mlra, ]
  # pull ids
  ids <- grids$Unique_ID
  # use this to gather paths and read in rasters
  rasts <- sprc()
  for (i in ids) {
    p1 <- sel10m[grepl(pattern = paste0(i, "_", year, ".tif"), x = sel10m)]
    rasts <- c(rasts, terra::rast(p1))
  }
  return(rasts)
}

exportTOF <- "temp/mlra80_TOF.tif"
if (!file.exists(exportTOF)) {
  # single raster for the AOI
  rasters <- gatherAllTOF(
    mlra = m1,
    g12 = g12,
    sel10m = tof10m,
    year = "2020"
  ) |>
    terra::mosaic(fun = "max")
  #export
  terra::writeRaster(x = rasters, filename = exportTOF)
} else {
  rasters <- terra::rast(exportTOF)
}


# processing TOF calculations  -------------------------------------------
processTOF_areas <- function(gridID, grids1k, cotData) {
  # 1. Select the specific grid cell
  g1 <- grids1k[grids1k$id == gridID, ]

  # 2. Wrap spatial processing in tryCatch
  result_df <- tryCatch(
    {
      # terra::intersect on extents returns NULL if they don't touch
      if (is.null(terra::intersect(terra::ext(g1), terra::ext(cotData)))) {
        stop("No overlap") # Force the error handler to run
      }

      # --- Primary Processing ---
      # Process rip to area
      r2 <- cotData |>
        terra::crop(g1) |>
        terra::mask(g1)

      # Check if r2 actually has values after masking (could be empty if polygon is between pixels)
      if (terra::ncell(r2) == 0) {
        stop("Empty raster after crop")
      }

      # Generate a cell size raster
      fullArea <- terra::cellSize(x = r2, unit = "km")
      ripArea <- r2 * fullArea

      # Calculate the total area and riparian area
      totArea <- sum(terra::values(fullArea), na.rm = TRUE)
      rArea <- sum(terra::values(ripArea), na.rm = TRUE)

      # Return valid dataframe
      data.frame(
        gridID = g1$id,
        tofArea = rArea,
        totalArea = totArea,
        percentTOF = (rArea / totArea) * 100
      )
    },
    error = function(e) {
      # --- Error Handler ---
      # This runs if crop fails (no overlap) or any other error occurs.
      # Returns a "Zero" row so your loop/map function doesn't break.
      return(data.frame(
        gridID = g1$id,
        tofArea = NA,
        totalArea = NA,
        percentTOF = NA # or NA, depending on your preference
      ))
    }
  )

  return(result_df)
}

# run the method
if (!file.exists("temp/tofArea_mlra80.csv")) {
  # apply processing
  tofAreas <- purrr::map(
    .x = k1$id,
    .f = processTOF_areas,
    grids1k = k1,
    cotData = rasters
  ) |>
    dplyr::bind_rows()
  write_csv(tofAreas, file = "temp/tofArea_mlra80.csv")
} else {
  tofAreas <- readr::read_csv("temp/tofArea_mlra80.csv")
}


# format datasets for joins and analysis  --------------------------------
ripZ <- out1 |>
  dplyr::select(
    gridID,
    percentRiparian
  ) |>
  addZscoreMetrics("percentRiparian")
## nlcd - group into categories
## start with lumping into the 8 categories
nlcd_cols <- c(
  "Water",
  "Developed",
  "Barren",
  "Forest",
  "Shrubland",
  "Herbaceous",
  "Planted",
  "Wetlands"
)

sum_cols <- function(...) {
  rowSums(pick(...), na.rm = TRUE)
}
nlcd8 <- out2 |>
  mutate(
    Water = sum_cols(starts_with("class_1")),
    Developed = sum_cols(starts_with("class_2")),
    Barren = sum_cols(starts_with("class_3")),
    Forest = sum_cols(starts_with("class_4")),
    Shrubland = sum_cols(starts_with("class_5")),
    Herbaceous = sum_cols(starts_with("class_7")),
    Planted = sum_cols(starts_with("class_8")),
    Wetlands = sum_cols(starts_with("class_9"))
  ) |>
  # Optional: Remove the original class columns to clean up
  select(-starts_with("class_")) |>
  addZscoreMetrics(target_cols = nlcd_cols)

# TOF measurements
tofZ <- tofAreas |>
  dplyr::select(gridID, gridArea = totalArea, percentTOF) |>
  addZscoreMetrics("percentTOF")

# process road
roadZ <- roadLength |>
  addZscoreMetrics("roadLengthKM")
## join all features
joinData <- tofZ |>
  dplyr::left_join(ripZ, by = "gridID") |>
  dplyr::left_join(nlcd8, by = "gridID") |>
  dplyr::left_join(roadZ, by = "gridID") |>
  dplyr::select(
    "gridID",
    "gridArea",
    "year",
    everything()
  )

# export
write_csv(joinData, "temp/allZScore_mlra80.csv")


# test for correlation between features and percentTOF
numeric_cols <- joinData |>
  dplyr::select_if(is.numeric) |>
  dplyr::select(-contains("_Zscore"))
# correlation of all columns against 'percentTOF'
cor_results <- cor(numeric_cols, use = "complete.obs")[, "percentTOF"]
cor_results <- sort(abs(cor_results), decreasing = TRUE)

# results
print(cor_results)


# based on this going to selec the top three elements for future evaluation
## forest, percentRiparian wetlands

# sampling methodology  ---------------------------------------------------

joinData <- read_csv("temp/allZScore_mlra80.csv")

# goals
## - generate a R square to compare the tof and
var(joinData$percentTOF, na.rm = TRUE)

### testing
# select tof and percent forest
df <- joinData |>
  dplyr::select(
    "gridID",
    "percentTOF",
    "Forest"
  ) # remove NA measures from the forest layer
# dplyr::filter(is.finite(Forest)) |>

# removed grids
removedGrids <- df[is.na(df$Forest), "gridID"]


# Set seed for reproducible K-means results
set.seed(123)
# generate a few different group to supply the neyman analysis
df <- joinData |>
  dplyr::select(gridID, percentTOF, Forest) |>
  # Remove NA/Inf values first so clustering works
  dplyr::filter(is.finite(Forest)) |>
  mutate(
    #Equal Interval - equal spacing on forest numbers
    group_equal = cut(
      Forest,
      breaks = 3,
      include.lowest = TRUE,
      labels = FALSE
    ),
    #Quantile - equal number of features per group
    group_quantile = cut(
      Forest,
      breaks = unique(quantile(Forest, probs = seq(0, 1, length.out = 4))),
      include.lowest = TRUE,
      labels = FALSE
    ),
    group_customQuantile = case_when(
      Forest == 0 ~ 1, # All zeros go to Group 1
      Forest <= median(Forest[Forest > 0]) ~ 2, # Below median of positives -> Group 2
      TRUE ~ 3 # Everything else -> Group 3
    ),
    #K-Means
    group_kmeans = {
      # Run K-means inside the pipe
      km <- kmeans(Forest, centers = 3)
      # Reorder the resulting cluster IDs (1,2,3) based on the actual Forest values
      as.integer(fct_reorder(as.factor(km$cluster), Forest, .fun = mean))
    }
  )
# nh = n * (Nh X Sh) / sum(Ni X Si)

# where:
#     nh is the sample size for stratum h
#     n is the total sample size
#     Nh is the population size for stratum h
#     Sh is the standard deviation of the variable of interest in stratum h
#     Σ represents the sum over all strata

## testing the method

n <- nrow(df)
# numerator for g1
g1 <- df[df$group_equal == 1, ]
Nh1 <- nrow(g1)
Sh1 <- sd(g1$percentTOF, na.rm = TRUE)
numerator1 <- Nh1 * Sh1
# numerator for g1
g2 <- df[df$group_equal == 2, ]
Nh2 <- nrow(g2)
Sh2 <- sd(g2$percentTOF, na.rm = TRUE)
numerator2 <- Nh2 * Sh2

# numerator for g1
g3 <- df[df$group_equal == 3, ]
Nh3 <- nrow(g3)
Sh3 <- sd(g3$percentTOF, na.rm = TRUE)
numerator3 <- Nh3 * Sh3
# demonorator
dem <- sum(numerator1, numerator2, numerator3)

tot1 <- n * (numerator1 / dem)
tot2 <- n * (numerator2 / dem)
tot3 <- n * (numerator3 / dem)
# sum of all these are equal to n (8801)
tot1 + tot2 + tot3
# if we just want the proportion we
pro1 <- (numerator1 / dem)
pro2 <- (numerator2 / dem)
pro3 <- (numerator3 / dem)


# using 100 here but in the sampling method, we will actually treat these as percentage sampled.
TOTAL_N <- 100

# function for calculating the neyman allocation
get_neyman_allocation <- function(
  data,
  strata_col,
  target_col,
  total_n,
  min_n = 2,
  splitType
) {
  #
  # Calculate Neyman Statistics
  result <- data |>
    group_by({{ strata_col }}) |>
    summarise(
      Nh = n(), # Stratum Size
      Sh = sd({{ target_col }}, na.rm = TRUE), # Standard Deviation
      mean_target = mean({{ target_col }}, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      # Calculate Neyman Weight (Nh * Sh)
      product = Nh * Sh,
      allocation_weight = product / sum(product),

      # 3. Initial Allocation
      n_raw = round(total_n * allocation_weight),

      # 4. Apply Constraints
      # Must have at least 'min_n' (usually 2) for variance calc
      # Cannot exceed 'Nh' (available samples)
      n_final = pmax(n_raw, min_n),
      n_final = pmin(n_final, Nh),

      # 5. Calculate final Sampling Density
      density = n_final / Nh
    ) |>
    dplyr::mutate(split = splitType) |>
    # Clean up helper columns
    select(group = {{ strata_col }}, everything(), -product)

  return(result)
}

# K-Means groups
neyman_kmeans <- get_neyman_allocation(
  data = df,
  strata_col = group_kmeans, # Note: No quotes needed
  target_col = percentTOF,
  total_n = 100,
  splitType = "kmeans"
)

# Equal Interval groups
neyman_equal <- get_neyman_allocation(
  data = df,
  strata_col = group_equal,
  target_col = percentTOF,
  total_n = 100,
  splitType = "equalInterval"
)
# Equal counts groups
neyman_quantile <- get_neyman_allocation(
  data = df,
  strata_col = group_quantile,
  target_col = percentTOF,
  total_n = 100,
  splitType = "quantile"
)
# zero plus quantile counts
neyman_zero_quantile <- get_neyman_allocation(
  data = df,
  strata_col = group_customQuantile,
  target_col = percentTOF,
  total_n = 100,
  splitType = "zero_Quantile"
)

# function for running the group stratified sample
get_circular_indices <- function(start, interval, n_sample, total_rows) {
  raw_seq <- seq(from = start, by = interval, length.out = n_sample)
  # The modulo math to wrap around
  return((raw_seq - 1) %% total_rows + 1)
}
# funtion for assigning the number of features to test in the iteration
increase_sample_size <- function(data, multiplier) {
  data %>%
    mutate(
      # Multiply n_final by the input
      # pmin() ensures the result never exceeds the value in Nh
      n_final = pmin(n_final * multiplier, Nh)
    )
}


# single dataset run  ----------------------------------------------------
# 2. Main Simulation Function
run_neyman_simulation <- function(
  data,
  allocation_df,
  total_area,
  iterations,
  seed = 1234,
  nth
) {
  set.seed(seed)
  # alter the total sample based on the iteration of the loop
  allocation_df <- increase_sample_size(data = allocation_df, multiplier = nth)
  # funtion to assing the
  # Join the allocation target (n_final) to the main data
  grouped_list <- data %>%
    inner_join(
      # Select dynamic column and n_final
      allocation_df %>% select(group, n_final),
      # Join using the string version of the name
      by = "group"
    ) %>%
    # Group by the dynamic column
    group_by(group) %>%
    mutate(
      gID = row_number(),
      n_total = n()
    ) %>%
    ungroup() %>%
    # Split by the dynamic column
    group_split(group)

  # SIMULATION LOOP:
  # map_dfr is a clean tidyverse alternative to the 'for' loop
  results <- map_dfr(1:iterations, function(i) {
    # Iterate over every group in our list (g1, g2, g3...)
    daily_sample <- map_dfr(grouped_list, function(group_df) {
      # Extract params specific to this group
      target_n <- group_df$n_final[1]
      total_n <- group_df$n_total[1]

      # Determine Interval
      interval <- round(total_n / target_n)

      # Random Start Point (The stochastic part)
      start_node <- sample(1:total_n, 1)

      # Get Indices
      idx <- get_circular_indices(start_node, interval, target_n, total_n)

      # Return the subset for this group
      return(group_df[idx, ])
    })

    # Calculate weighted mean for the combined sample
    sample_ratio <- weighted.mean(
      x = daily_sample$percentTOF,
      w = daily_sample$areas,
      na.rm = TRUE
    )

    # Return result row
    data.frame(
      iteration = i,
      tofEstimate = (sample_ratio / 100) * total_area
    )
  })

  return(results)
}

df2 <- df_t

data <- run_neyman_simulation(
  data = df_t,
  allocation_df = neyman_kmeans,
  total_area = totalArea,
  iterations = 20,
  nth = 10
)


# Itorative runs over all the datasets  ----------------------------------

# filter the k1 data to exclude any feautures that 0had NA for previous measure value
k2 <- k1[!k1$id %in% removedGrids$gridID, ]

k2$areas <- terra::expanse(k2, unit = "km")

# calculate the total areas
totalArea <- sum(k2$areas, na.rm = TRUE)
# simplified version for joining to sample data
k3 <- k2 |> as.data.frame() |> dplyr::select(id, areas)

# remove the na columns from the df
df_t <- df |>
  dplyr::filter(!gridID %in% removedGrids$gridID) |>
  dplyr::left_join(k3, by = c("gridID" = "id"))

# total percent TOF
# calculate the weighted mean of
fullRatio <- weighted.mean(
  x = df_t$percentTOF,
  w = df_t$areas,
  na.rm = TRUE
)
# TOF area estimate - convert to a percentage
tofMeasured <- (fullRatio / 100) * totalArea
tenPercent <- tofMeasured * 0.1
low <- tofMeasured - tenPercent
high <- tofMeasured + tenPercent

# only want to run up to divisable iterations of 100
runMax <- floor(nrow(df_t) / 100)
successfulRuns <- c()
runIterations <- 20


data_list <- list(
  equal = list(data = neyman_equal, name = "group_equal"),
  kmeans = list(data = neyman_kmeans, name = "group_kmeans"),
  quantile = list(data = neyman_quantile, name = "group_quantile"),
  zeroQuantile = list(
    data = neyman_zero_quantile,
    name = "group_customQuantile"
  )
)


runSampling_neyman <- function(data_list, tofDF, runIterations) {
  # data
  data <- data_list

  #  run the method for the specific dataset
  method <- data$name
  valsDF <- data$data
  # set the number of possible runs based on rows in tofDF
  runMax <- floor(nrow(tofDF) / 100)
  # filter the group df to the specific method
  df2 <- tofDF |>
    dplyr::select(
      "gridID",
      "percentTOF",
      "Forest",
      group = {{ method }},
      "areas"
    )
  resultsDF <- data.frame(
    runNumber = 1:runMax,
    percentTRUE = NA,
    method = method
  )
  for (i in 1:runMax) {
    # set.seed(i)
    print(i)
    # Run the simulation
    outputVals <- run_neyman_simulation(
      data = df2,
      allocation_df = valsDF,
      total_area = totalArea,
      iterations = runIterations,
      nth = i
    )
    # assgin inrange test
    ranges <- outputVals |>
      dplyr::mutate(
        inRange = case_when(
          tofEstimate >= low & tofEstimate <= high ~ TRUE,
          TRUE ~ FALSE
        )
      )
    # store the results of the process
    resultsDF$percentTRUE[i] <- (sum(ranges$inRange) / runIterations) * 100
  }
  return(resultsDF)
}

results2 <- purrr::map_dfr(
  .x = data_list,
  .f = runSampling_neyman,
  tofDF = df_t,
  runIterations = 20
)
