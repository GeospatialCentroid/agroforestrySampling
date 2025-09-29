# attempt at modeling the predition of the TOF cover based on known nlcd landcover of the region 

pacman::p_load(dplyr, reshape2, terra, data.table)

# test inputs 
landCover <- read_csv("data/derived/nlcdSummaryAreas/largeClass_summaryMLRA.csv")
sampling <- read_csv("data/derived/samplingXTesting/aggregated/mlra80_10.csv")
# assign id for joining data 
landCover$id <- as.numeric(sub("mlra_", "", landCover$areaName))
# id 63 and 89 have very low coverage areas so I'm removing the the training data 
landCover <- landCover[!landCover$id %in% c(63,89), ]
sampling <- sampling[!sampling$id %in% c(63,89), ] |>
  dplyr::filter(iterations == 200)
# convert table to wide 
landCoverWide <- dcast(landCover, id ~ lcGroup, value.var = "percentArea", fun.aggregate = mean)



# join the data
merged <- merge(sampling, landCoverWide, by = "id")

model_data <- merged %>%
  select(samplePercentage, Barren, Developed, Forest, Herbaceous,
         cultived = `Planted/Cultivated`,
         Shrubland, Water) |>
  dplyr::distinct()

# Handle potential missing values - let's omit rows with NAs for simplicity
model_data <- na.omit(model_data)


# 5. Build the Linear Regression Model
# We are predicting samplePercentage based on the land cover percentages
model <- lm(samplePercentage ~ ., data = model_data)

summary(model)




# 6. Add Predictions to Data
# We can add the model's predictions to our dataframe for plotting
model_data$predictedPercentage <- predict(model, model_data)


# 7. Plot the Results

# Plot 1: Predicted vs. Actual Values
ggplot(model_data, aes(x = samplePercentage, y = predictedPercentage)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Predicted vs. Actual samplePercentage",
    x = "Actual Values (from data)",
    y = "Predicted Values (from model)"
  ) +
  theme_minimal() +
  coord_fixed(ratio = 1) # Ensures the x and y axes have the same scale


# Plot 2: Coefficient Importance Plot
# Extract coefficient information from the model summary
coeff_summary <- summary(model)$coefficients
coeff_df <- as.data.frame(coeff_summary)
# Add variable names as a column
coeff_df$Variable <- rownames(coeff_df)
# Remove the '(Intercept)' for better visualization
coeff_df <- coeff_df %>% filter(Variable != "(Intercept)")


ggplot(coeff_df, aes(x = reorder(Variable, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), width = 0.2) +
  coord_flip() +
  labs(
    title = "Importance of Land Cover Types",
    subtitle = "Coefficients from the linear model with standard errors",
    x = "Land Cover Type",
    y = "Coefficient Estimate"
  ) +
  theme_minimal()


# rf iterations  ----------------------------------------------------------

library(randomForest)

# Set a seed for reproducibility
set.seed(123)

# Train the Random Forest model
model_rf <- randomForest(samplePercentage ~ ., data = model_data, ntree = 500, mtry = 3)

# Print the model to see its performance (Mean of squared residuals and % Var explained)
print(model_rf)

# You can also create an importance plot
importance(model_rf)
varImpPlot(model_rf)
# add prediction 
model_data$predicted_rf <- predict(model_rf, newdata = model_data)

# Plot 1: Predicted vs. Actual Values
ggplot(model_data, aes(x = samplePercentage, y = predicted_rf)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Random Forest Model: Predicted vs. Actual",
    subtitle = "Points should be close to the red dashed line for a good fit",
    x = "Actual Values (from data)",
    y = "Predicted Values (from model)"
  ) +
  theme_minimal() +
  coord_fixed(ratio = 1)






# Create a new dataframe with the land cover data for a new MLRA
new_mlra_data <- data.frame(
  Barren = 0.2,
  Developed = 5.0,
  Forest = 2.0,
  Herbaceous = 15.0,
  cultived= 75.0,
  Shrubland = 0.1,
  Water = 0.5
)

# Use the model to predict the samplePercentage
predicted_percentage <- predict(model, newdata = new_mlra_data)

# Print the prediction
print(predicted_percentage)

