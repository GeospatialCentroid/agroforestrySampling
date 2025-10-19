# 1. Generate sample data
# This step is just to make the example reproducible.
# In your actual use case, you would load your own data frame.
set.seed(42) # for reproducibility
n_points <- 50

# Create a data frame with two variables: riparian_area and canopy_cover
# We'll assume a positive correlation for this example.
dat <- data.frame(
  riparian_area = runif(n_points, min = 0, max = 100) # predictor variable (x)
)
dat$canopy_cover <- dat$riparian_area * 0.7 + rnorm(n_points, mean = 0, sd = 10) # outcome variable (y)

# Ensure canopy cover is within a realistic range (e.g., 0 to 100)
dat$canopy_cover <- pmax(0, pmin(100, dat$canopy_cover))


# 2. Create the linear model
# The lm() function fits a linear model. The formula 'y ~ x' means y is modeled by x.
model <- lm(canopy_cover ~ riparian_area, data = dat)


# 3. Generate the plot
plot(
  x = dat$riparian_area,
  y = dat$canopy_cover,
  main = "Canopy Cover vs. Riparian Area",
  xlab = "Riparian Area (e.g., in square meters)",
  ylab = "Canopy Cover (%)",
  pch = 16, # Use solid circles for points
  col = "steelblue"
)

# 4. Add the regression line to the existing plot
# The abline() function can take a model object and draw the corresponding line.
abline(model, col = "red", lwd = 2)

# 5. (Optional) Add a legend to the plot
legend("topleft", 
       legend = c("Data Points", "Regression Line"), 
       col = c("steelblue", "red"), 
       pch = c(16, NA), # Point character for the data points
       lty = c(NA, 1),    # Line type for the regression line
       lwd = c(NA, 2),    # Line width for the regression line
       bty = "n"          # No box around legend
)