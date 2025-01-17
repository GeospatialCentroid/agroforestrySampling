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


# generate percentages 
results2 <- results |>
  dplyr::mutate(
    percent10 = (trees2010/totalArea)*100,
    percent16 = (trees2016/totalArea)*100,
    percent20 = (trees2020/totalArea)*100
    
  )
View(results)


# produce a plot ----------------------------------------------------------
library(plotly)
# Create a long format data frame for plotting
df_long <- results2 %>%
  select(name, percent10, percent16, percent20) %>%
  tidyr::pivot_longer(cols = -name, names_to = "year", values_to = "percent_cover")

# Create the Plotly bar chart
plot_ly(df_long, x = ~name, y = ~percent_cover, color = ~year, type = "bar")%>%
  layout(
    title = "Percent Tree Cover Over Time",
    xaxis = list(title = "Ecoregion Name"),
    yaxis = list(title = "Percent Tree Cover"),
    barmode = "group"  # Grouped bar chart
  )

# do a quite map to show locations of the eco regions 
library(leaflet)
# read in nebrasksa area 
neb <- terra::vect("data/products/modelGrids_2010.gpkg")
# read in ecoregions 
eco <- terra::vect("data/raw/spatialAreaFiles/ecoregions/us_eco_l3.gpkg")
ecoSel <- eco[eco$US_L3NAME %in% results2$name, ] |>
  terra::crop(neb)
# Define a color palette for the ecoregions
pal <- colorFactor(palette = "viridis", domain = ecoSel$US_L3NAME)

# Create the Leaflet map
leaflet(ecoSel) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(US_L3NAME),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~US_L3NAME,  # Label each ecoregion with its name
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal, values = ~US_L3NAME, opacity = 0.7, title = "Ecoregions",
    position = "bottomright"
  )
