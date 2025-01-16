###
# developing a method for aggregating values from a list of csvs
#
#
### 

# Load necessary libraries
pacman::p_load(dplyr, readr) 

# Function to convert square meters to hectares
sq_m_to_ha <- function(sq_meters) {
  hectares <- sq_meters / 10000  # 1 hectare = 10,000 square meters
  return(hectares)
}

# Function to convert square kilometers to hectares
sq_km_to_ha <- function(sq_km) {
  hectares <- sq_km * 100  # 1 square kilometer = 100 hectares
  return(hectares)
}


## This function processes data based on the folder naming structure 
aggregateResults <- function(folder){
  
  # Define export path for the aggregated results
  exportPath <- paste0(folder,"/tof_areaSummary.csv") 
  
  # Check if the summary file already exists
  if(!file.exists(exportPath)){ 
    # List all CSV files in the specified folder
    files <- list.files(path = folder, 
                        pattern = "pixelCounts_",  # Only files with pixelCounts_ to avoid existing summary files
                        full.names = TRUE)  # Get full file paths
    # Extract unique area names from the file paths
    # This assumes the area name is the third element after splitting by "_"
    # (e.g., "pixelCounts_X12-82_High Plains_.csv" -> "High Plains")
    names <- unique(sapply(strsplit(basename(files), "_"), function(x) x[3])) 
    
    # Create an empty data frame to store the aggregated results
    df <- data.frame(matrix(nrow = (length(names)), ncol = 7)) 
    names(df) <- c("areaFeature", "name", "numberOfGrids", "totalArea",'trees2010', "trees2016", "trees2020")
    df$areaFeature <- basename(folder)  # Set the areaFeature column
    
    # Loop through each unique area name
    for(i in seq_along(names)){  
      print(paste0("processing area ",i, " of ", length(names)))
      
      # Assign the current area name
      name <- names[i] 
      
      # Select all files related to the current area name
      f2 <- files[grepl(name, files)] |>  # Filter files containing the name
        read_csv() |>  # Read the selected files into a data frame
        dplyr::summarise(  # Calculate summary statistics
          girdCount = n(),  # Count the number of grids
          totalArea = sq_km_to_ha(sum(newArea, na.rm = TRUE)),  # Calculate total area in hectares
          cell10 = sq_m_to_ha(sum(cells2010, na.rm = TRUE)),  # Calculate area of trees in 2010 in hectares
          cell16 = sq_m_to_ha(sum(cells2016, na.rm = TRUE)),  # Calculate area of trees in 2016 in hectares
          cell20 = sq_m_to_ha(sum(cells2020, na.rm = TRUE))   # Calculate area of trees in 2020 in hectares
        )
      
      # Assign the calculated values to the data frame
      df[i, "name"] <- name
      df[i,3:7] <- f2[1,1:5] 
    }
    
    print("exporting data")
    
    # Export the aggregated results to a CSV file
    write_csv(x = df, file = exportPath) 
    
  } else {  # If the summary file already exists
    print("reading existing data")
    
    # Read the existing summary file
    df <- read_csv(exportPath) 
  }
  
  # Return the aggregated data frame
  return(df)  
}


# Run the function 

# Define the folder containing the CSV files
folder <- "data/derived/areaCounts/ecoRegionLevel3" 

# Call the aggregateResults function and store the results
results <- aggregateResults(folder)
