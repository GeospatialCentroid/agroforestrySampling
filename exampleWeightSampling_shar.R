library(dplyr)

df4 <- data.frame(
  ID = 1:4, 
  area = c(10,20,30,40),
  TOF = runif(4, min = 0.02, max = 0.50)
)
df20 <- data.frame(
  ID = 1:20, 
  area = runif(20, min = 10, max = 40),
  TOF = runif(20, min = 0.02, max = 0.50)
)


# ---------------------------
# Ratio estimator
# ---------------------------
total_area_mlra = sum(df4$area) # df4["cell_area_m2"].sum()  # total MLRA area
mean_ratio = sample_cells["tree_area_m2"].sum() / sample_cells["cell_area_m2"].sum()
estimated_total_tree_area = mean_ratio * total_area_mlra

# Variance & 95% CI
residuals = sample_cells["tree_area_m2"] - mean_ratio * sample_cells["cell_area_m2"]
var_ratio = np.sum(residuals**2) / (len(sample_cells) - 1)
SE = np.sqrt((1 - N / len(grid_clipped)) * var_ratio / N) * total_area_mlra
CI_lower = estimated_total_tree_area - 1.96 * SE
CI_upper = estimated_total_tree_area + 1.96 * SE

# ---------------------------
# Output
# ---------------------------
print(f"Estimated tree cover area: {estimated_total_tree_area/1e6:.2f} km²")
print(f"95% CI: [{CI_lower/1e6:.2f}, {CI_upper/1e6:.2f}] km²")
print(sample_cells[['Unique_ID', 'cell_area_m2', 'tree_area_m2']])

