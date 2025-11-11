pacman::p_load(
  dplyr,
  terra,
  purrr,
  tidyr,
  readr,
  targets,
  corrr,
  ggplot2,
  plotly,
  car
)


# Compare the landscape characteristics within the 1km grids against all the grids in the MLRA

# sub grid data ----------------------------------------------------------
# mlra tofs-csv
mlraTOF <- list.files(
  "temp/subGridTOF",
  pattern = "mlra_72.csv",
  full.names = TRUE
) |>
  read_csv()

ripArea_sugGrids <- tar_read(ripArea_sugGrids) |>
  dplyr::filter(MLRA_ID == 72)
nlcd_10_sugGrids <- tar_read(nlcd_10_sugGrids) |>
  dplyr::filter(MLRA_ID == 72)
nlcd_16_sugGrids <- tar_read(nlcd_16_sugGrids) |>
  dplyr::filter(MLRA_ID == 72)
nlcd20_by_nlcd_20_sugGridsmlra <- tar_read(nlcd_20_sugGrids) |>
  dplyr::filter(MLRA_ID == 72)

# focus on 2016 for now
## TOF
m16 <- mlraTOF |>
  dplyr::select(id, gridArea, tof2016_sum)
## riparian area
r16 <- ripArea_sugGrids |>
  dplyr::select(id, starts_with("totalCount_")) %>%
  dplyr::mutate(
    total_sum = rowSums(select(., -c(id, totalCount_NA)), na.rm = TRUE)
  ) |>
  dplyr::select(id, noRiparian = totalCount_NA, riparianArea = total_sum)

## NLCD
n16 <- nlcd_16_sugGrids |>
  dplyr::select(id, starts_with("totalCount")) |>
  dplyr::rename(
    Open_Water = totalCount_11,
    Developed_Open_Space = totalCount_21,
    Developed_Low_Intensity = totalCount_22,
    Developed_Medium_Intensity = totalCount_23,
    Developed_High_Intensity = totalCount_24,
    Barren_Land = totalCount_31,
    Deciduous_Forest = totalCount_41,
    Evergreen_Forest = totalCount_42,
    Mixed_Forest = totalCount_43,
    Shrub_Scrub = totalCount_52,
    Grassland_Herbaceous = totalCount_71,
    Pasture_Hay = totalCount_81,
    Cultivated_Crops = totalCount_82,
    Woody_Wetlands = totalCount_90,
    Emergent_Herbaceous_Wetlands = totalCount_95
  ) |>
  dplyr::select(-totalCount_NA)


# join all the dataset together
df <- m16 |>
  dplyr::left_join(r16, by = "id") |>
  dplyr::left_join(n16, by = "id")

# Test correlation between log-transformed TOF and log-transformed characteristics
log_correlation_results <- df |>
  # Select only the numeric columns to correlate
  select(-id, -gridArea) |>
  # Apply the log-plus-one transformation to ALL selected columns
  mutate(across(everything(), ~ log(.x + 1))) |>
  # Compute the correlation matrix on the log-transformed data
  correlate(use = "pairwise.complete.obs") |>
  # Zero in on the (now log-transformed) 'tof2016_sum' column
  focus(tof2016_sum) |>
  # Remove the self-correlation
  filter(term != "tof2016_sum_log") |>
  # Create a new column for absolute correlation strength
  mutate(abs_cor = abs(tof2016_sum)) |>
  # Sort by the strongest correlation
  arrange(desc(abs_cor))

# --- Print the new results ---
print(log_correlation_results)
no
# --- Show the top 3 correlated variables ---
top_3_cors <- log_correlation_results |>
  slice_head(n = 3)
top_3_vars <- top_3_cors$term

# 1. CALCULATE R-SQUARED FOR EACH *LOG-TRANSFORMED* VARIABLE
r_squared_results <- map_dfr(
  top_3_vars,
  ~ {
    # --- KEY CHANGE HERE ---
    # Create the formula for the LOG-LOG model
    formula_str <- paste0("log(tof2016_sum + 1) ~ log(`", .x, "` + 1)")
    model_formula <- as.formula(formula_str)

    # Run the linear model (on the fly transformations)
    model <- lm(model_formula, data = df)

    # Get the summary
    model_summary <- summary(model)

    # Return a one-row tibble with the results
    tibble(
      land_cover_variable = .x, # This name is important for joining
      r_squared = model_summary$r.squared
    )
  }
)

# --- Print the calculated R-squared values ---
print("Log-Log Model R-squared results:")
print(r_squared_results)


# 2. CREATE THE *LOG-LOG* PLOT WITH R-SQUARED LABELS

# First, create a text label from the R-squared results
plot_labels <- r_squared_results |>
  mutate(
    r_squared_label = paste0("R² = ", round(r_squared, 3))
  )

# Next, pivot the *original* data for plotting
# (The transformation will happen in ggplot)
df_long_for_plot <- df |>
  pivot_longer(
    cols = all_of(top_3_vars),
    names_to = "land_cover_variable",
    values_to = "pixel_count"
  )

# --- Create the plot ---
top_3_plot_with_rsq <- df_long_for_plot |>

  # --- KEY CHANGE HERE ---
  # Apply the log transformation directly in the 'aes()'
  ggplot(aes(x = log(pixel_count + 1), y = log(tof2016_sum + 1))) +

  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "blue") +

  # Add the R-squared text
  # This works because 'plot_labels' has the R² for the log-log model
  geom_text(
    data = plot_labels,
    aes(x = -Inf, y = Inf, label = r_squared_label),
    hjust = -0.1,
    vjust = 1.5,
    inherit.aes = FALSE
  ) +

  # Facet as before
  facet_wrap(~land_cover_variable, scales = "free_x") +

  # --- KEY CHANGE HERE ---
  # Update labels to show the transformation
  labs(
    title = "Top 3 Predictors vs. tof2016_sum (Log-Log Transformed)",
    y = "log(tof2016_sum + 1)",
    x = "log(Variable Pixel Count + 1)"
  ) +
  theme_minimal()

# --- Display the plot ---
print(top_3_plot_with_rsq)


# generate groups --------------------------------------------------------

#' Test Variance by Groups
#'
#' Takes a dataframe, a Y variable, and an X variable,
#' splits the data into groups based on the X variable,
#' and then calculates variance and runs a Levene's test
#' on the Y variable for those groups.
#'
#' @param data The input dataframe.
#' @param y_var The (unquoted) column name of the variable to test (e.g., tof2016_sum).
#' @param x_var The (unquoted) column name to group by (e.g., Grassland_Herbaceous).
#' @param n_groups The number of groups (terciles, quartiles, etc.) to create.
#'
#' @return A list containing 'variance_table' and 'levene_test' results.

ntile_group_variance <- function(data, y_var, x_var, n_groups = 3) {
  # --- Step 1: Create Grouped Data ---
  # We use the {{ }} (curly-curly) operator to pass the
  # unquoted variable names to dplyr functions.
  grouped_data <- data |>
    # Mutate to create the pixel_group column
    dplyr::mutate(
      # Create n-tile groups based on the x_var
      # and immediately turn it into a factor for the test
      pixel_group = factor(dplyr::ntile({{ x_var }}, n_groups))
    )

  # --- Step 2: Calculate Variance Table ---
  variance_table <- grouped_data |>
    dplyr::group_by(pixel_group) |>
    dplyr::summarise(
      # Calculate variance for the y_var, remove NAs
      n_obs = n(),
      variance = var({{ y_var }}, na.rm = TRUE),
      mean = mean({{ y_var }}, na.rm = TRUE)
    )
  return(variance_table)
}

# kmeans clusting version
#' Test Variance by K-Means Groups
#'
#' Takes a dataframe, a Y variable, and an X variable,
#' splits the data into k-means clusters based on the X variable,
#' and then calculates the variance of the Y variable for those groups.
#'
#' @param data The input dataframe.
#' @param y_var The (unquoted) column name of the variable to test (e.g., tof2016_sum).
#' @param x_var The (unquoted) column name to cluster by (e.g., Grassland_Herbaceous).
#' @param n_groups The number of clusters (k) to create.
#'
#' @return A tibble (variance table).

library(dplyr)

#' Test Variance by K-Means Groups
#'
#' @param data The input dataframe.
#' @param y_var The (unquoted) column name of the variable to test (e.g., tof2016_sum).
#' @param x_var The (unquoted) column name to cluster by (e.g., Grassland_Herbaceous).
#' @param n_groups The number of clusters (k) to create.
#'
#' @return A tibble (variance table).

kmeans_group_variance <- function(data, y_var, x_var, n_groups = 3) {
  # --- Step 1: Create Grouped Data using k-means ---
  data_filtered <- data |>
    dplyr::filter(!is.na({{ x_var }}))

  x_vector <- data_filtered |>
    dplyr::pull({{ x_var }})

  kmeans_result <- stats::kmeans(x_vector, centers = n_groups, nstart = 25)

  grouped_data <- data_filtered |>
    dplyr::mutate(
      pixel_group = factor(kmeans_result$cluster)
    )

  # --- Step 2: Calculate Variance Table ---
  variance_table <- grouped_data |>
    dplyr::group_by(pixel_group) |>
    dplyr::summarise(
      n_obs = n(),
      variance = var({{ y_var }}, na.rm = TRUE),
      mean = mean({{ y_var }}, na.rm = TRUE),

      # *** MOVED THIS LINE ***
      # Calculate the mean of the x_var *before* the summarize is finished
      group_mean_x = mean({{ x_var }}, na.rm = TRUE)
    ) |>
    # Now we can arrange by the column we just created
    dplyr::arrange(group_mean_x) |>
    dplyr::select(-group_mean_x) # Remove the helper column

  return(variance_table)
}


# --- Example 1: Run on your original (raw) data ---
# (Assuming your original dataframe is 'df')

ntile_variance_results <- ntile_group_variance(
  data = df,
  y_var = tof2016_sum,
  x_var = Emergent_Herbaceous_Wetlands
)
# To see the variance table:
print(ntile_variance_results)

# kmeans approach
kmeans_variance_results <- kmeans_group_variance(
  data = df,
  y_var = tof2016_sum,
  x_var = Emergent_Herbaceous_Wetlands
)

# To see the variance table:
print(kmeans_variance_results)


# --- Example 2: Run on your LOG-TRANSFORMED data ---
# This is the more statistically robust method we discussed.

# First, create the new log columns in your main 'df'
df_log <- df |>
  mutate(
    tof_log = log(tof2016_sum + 1),
    herb_log = log(Grassland_Herbaceous + 1)
  )

# Now, call the function using the new log-transformed columns
ntile_log_variance_results <- ntile_group_variance(
  data = df_log,
  y_var = tof_log,
  x_var = herb_log
)
print(ntile_log_variance_results)
kmeans_log_variance_results <- kmeans_group_variance(
  data = df_log,
  y_var = tof_log,
  x_var = herb_log
)
print(print())

# Print the results for the log-transformed data
print("--- Log-Transformed Variance Table ---")
print(log_variance_results$variance_table)

print("--- Log-Transformed Levene's Test ---")
print(log_variance_results$levene_test)
# (You'd hope to see a p-value > 0.05 here)

herb_wetlands <- log_correlation_results |>
  # Select just the columns we need for this analysis
  select(tof2016_sum, Grassland_Herbaceous) |>
  # Create a new column 'pixel_group' that splits the data into 3
  # groups (terciles) based on the 'Grassland_Herbaceous' value
  mutate(
    pixel_group = ntile(Grassland_Herbaceous, 3)
  ) |>
  # Convert the numeric group (1, 2, 3) to a named factor
  # This is best practice for statistical tests
  mutate(
    pixel_group = factor(
      pixel_group,
      levels = c(1, 2, 3),
      labels = c("Low_Count", "Medium_Count", "High_Count")
    )
  )
# You can check the new groups and their sizes
# print(herbaceous_grouped |> count(pixel_group))

# 2. Calculate and show the variance for each group
variance_table <- herbaceous_grouped |>
  group_by(pixel_group) |>
  summarise(
    variance_tof2016_sum = var(tof2016_sum, na.rm = TRUE)
  )

# 3. Print the simple table
print(variance_table)


# --- 3. Run the Test for Variability (Levene's Test) ---

# We'll test if the variance of 'tof2016_sum' is the same
# for all three 'pixel_group' levels.
levene_result <- leveneTest(
  tof2016_sum ~ pixel_group,
  data = herbaceous_grouped
)

# --- 4. Print the Result ---
print("Levene's Test for Homogeneity of Variances:")
print(levene_result)

# Requires the 'herbaceous_grouped' data from step 2
ggplot(
  herbaceous_grouped,
  aes(x = pixel_group, y = tof2016_sum, fill = pixel_group)
) +
  geom_boxplot() +
  labs(
    title = "Variability of tof2016_sum by Herbaceous Pixel Group",
    x = "Grassland_Herbaceous Pixel Count Group",
    y = "tof2016_sum"
  ) +
  scale_y_log10() + # Good idea if 'tof2016_sum' is highly skewed
  theme_minimal() +
  theme(legend.position = "none") # The x-axis is already labeled
