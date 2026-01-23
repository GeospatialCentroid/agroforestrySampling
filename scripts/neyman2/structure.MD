# Data Processing Structure

## Static Data Processing
### Inputs
- 12-mile grids
- 1 km grids
- NLCD 2010, 2016, 2020
- Riparian areas
- MLRA (Major Land Resource Areas)

### Methods
1. **Crop 1km grids to MLRAs**
   - Export 1km grids per MLRA.
2. **Attribute 12-mile grid IDs to each 1km area**
   - Create a dataframe with MLRA ID, 1km grid ID, and 12-mile grid ID.
     - This step is optional as the 12-mile gridded product will not be used going forward.
3. **Attribute riparian areas to all 1km grids per MLRA**
   - Create a dataframe with MLRA ID, 1km grid ID, total area, and percent area riparian.
4. **Attribute NLCD 2010, 2016, 2020 to all 1km grids per MLRA**
   - Create a dataframe with MLRA ID, 1km grid ID, total area, and percent area for 8 NLCD classes for 2010, 2016, and 2020.
5. **Combine dataframes**
   - Bind the riparian and NLCD data into a single dataframe from the MLRA.
     - The 12-mile grid dataframe should be optional.

## Dynamic Processing
### Inputs
- TOF (Time of Flight) measure for all 12-mile grids
- MLRA cropped 1km grids
- Combined dataframe from static processing

### Methods
1. **Extract the %TOF value for each 1km grid in the MLRA**
   - Create a dataframe with MLRA ID, grid ID, and %TOF for 2010, 2016, and 2020.
2. **Combine static processing data with %TOF data frame**
   - Create a single dataframe with MLRA ID, grid ID, %TOF, riparian, and NLCD values.

### Extra
- A function that produces a map of the average value of a parameter for all MLRAs of interest.
  - This can take in a list of MLRA IDs and a specific parameter (%TOF, riparian, NLCD class).

## Neyman Allocation
Here we need to be able to iterate over MLRA, year, and stratification variable.

### Inputs
- Dataframe for a specific MLRA (output of dynamic processing)

### Methods
1. **Generate stratification groups for the specific MLRA and parameter**
   - Use methods like k-means, equal interval, quantile, and zero-inflated quantile.
   - Export a CSV that shows MLRA ID, 1km grid ID, %TOF, parameter values, and groups for the four classes.
2. **Generate a chart showing variability in %TOF for each stratification group**
   - Display results in a table or graph.
3. **Summarize across all parameters for a specific year what stratification parameter combination produces the highest amount of variance in TOF**
   - Display results in a table or graph showing all potential options.
4. **Generate Neyman allocation values**
   - This is likely to be a dataframe.

## Sampling Methodology

