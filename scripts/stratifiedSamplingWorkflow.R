
  
pacman::p_load("terra", "dplyr", "readr", "sf", "tictoc", "tmap", "plotly", "broom", "rlang", "leaflet")
tmap_mode("view")
source("functions/samplingWorkflowFunctions.R")
# goal 
## want to generate a systematic sample of a AOI 
## these are iterative process that will continue until 
## 80% of the draws at a specific number result in value within +/- 10% of the know value 




# generate a leafetmap for visulizaitons  ---------------------------------
mlra <- getSpatialFiles(area = "MLRA")
lrr <- getSpatialFiles(area = "LRR")
map <- leaflet() %>%
  # --- Add Background Map Layers (Base Groups) ---
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addTiles(group = "OpenStreetMap") %>% # OSM is the default tile layer
  # --- Add Polygon Layers (Overlay Groups) ---
  addPolygons(
    data = mlra,
    color = "#0073C2FF",  # A distinct blue outline color
    weight = 2,           # Outline thickness
    fillOpacity = 0.5,
    group = "MLRA",  # Assign to a group for the toggle control
    label = ~MLRA_ID  # Text that appears on hover
  ) %>%
  addPolygons(
    data = lrr,
    color = "#EFC000FF",  # A distinct gold outline color
    weight = 2,
    fillOpacity = 0,
    group = "LRR", # Assign to a different group
    label = ~LRR_NAME 
  ) %>%
  # --- Add the Layer Control Widget ---
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Satellite"),
    overlayGroups = c("MLRA", "LRR"),
    options = layersControlOptions(collapsed = FALSE) # Keep the control open
  )
map
# stratified workflow  ----------------------------------------------------
grids <- getGrids(gridSize = 10)
# standard parameters
size <- "10k"
proportionValue <-  80

mlra <- FALSE
if(isTRUE(mlra)){
  featName <- "MLRA"
  export1 <- "data/derived/samplingXTesting/mlra80_"
  pattern1 <- "mlra80_"
  export2 <- "data/derived/samplingXTesting/aggregated/mlra80_iterations.csv"
  export3 <- "data/derived/samplingXTesting/aggregated/mlra80_"
}else{
  featName <- "LRR"
  export1 <- "data/derived/samplingXTesting/lrr80_"
  pattern1 <- "lrr80_"
  export2 <- "data/derived/samplingXTesting/aggregated/lrr80_iterations.csv"
  export3 <- "data/derived/samplingXTesting/aggregated/lrr80_"
}




# results at 80% agreement  ------------------------------------------------
## with 5,10,20 itorations of j : 25,43,78
## with 100 itorations of j : 76.915 sec elapsed
time <- c()
inter <- c(5,10,20,50,100,250,500,1000)
for(itor in 7:8){
  print(itor)
  tic()
  area80 <- purrr::map(.x = featName, .f = callStartitfied, size = "10k",
                       proportionValue =  80, iter = inter[itor], .progress = TRUE)
  time <- c(time, toc()$callback_msg)
  # if(itor == 1){
  #   results<- area80
  # }else{
  #   results <- c(results, area80)
  # }
  export <- paste0(export1,inter[itor],".csv")
  write_csv(x = area80[[1]], export)
}


# compile to single feature  ----------------------------------------------
f1 <- list.files("data/derived/samplingXTesting",
                 pattern = pattern1,
                 full.names = TRUE) 
# read in assign sample amount and combine 
for(i in seq_along(f1)){
  feat <- f1[i]
  base <- tools::file_path_sans_ext(basename(feat))|> 
    stringr::str_remove( pattern1)
  d1 <- read_csv(feat)|>
    dplyr::mutate(iterations = base)
  if(i == 1){
    result <- d1
  }else{
    result <- bind_rows(result, d1)
  }
}
# alter values to percent of areas rather than counts 
result1 <- result |> 
  mutate(
    sample10_percent = (sample10 / totalAreas) * 100,
    sample16_percent = (sample16 / totalAreas) * 100,
    sample20_percent = (sample20 / totalAreas) * 100,
    iterations = as.numeric(iterations)
  )


write_csv(result1, export2 )

# parse out results in to csv per year  -----------------------------------
select_by_suffix <- function(data, suffix) {
  sel <- data %>%
    select(id, totalAreas,iterations, contains(as.character(suffix)))
  names(sel) <- c("id", "totalAreas","iterations", "tof", "sample", "sampleTOF", "samplePercentage")
  return(sel)
}

for(i in c("10","16","20")){
  export <- paste0(export3,i,".csv")
  sel <- select_by_suffix(data = result1, suffix = i)
  write_csv(x = sel, file = export)
}

# generate the plot wit the lms  ------------------------------------------
create_lm_fit <- function(data, response_col) {
  
  # The '{{}}' operator allows us to use the response_col argument
  # as a variable for the column name.
  data$response <- data[[response_col]]
  
  data %>%
    # Now we can filter and model using that consistent name
    filter(!is.na(response)) %>%
    group_by(id) %>%
    do(augment(lm(response ~ iterations, data = .)))
}




# add a lm for plot 
result2 <-create_lm_fit(data = result1, response_col = "sample10_percent")
  


# Define the plotting function
plot_trends <- function(marker_data,  y_var) {
  
  # --- Step 1: Prepare variables for plotting ---
  # Capture the column name expression
  y_var_expr <- enquo(y_var)
  
  # Convert it to a string for use in titles
  y_var_string <- as_name(y_var_expr)
  
  
  # generate the lm 
  line_data <-create_lm_fit(data = marker_data, response_col = y_var_string)
  
  
  
  # --- Step 2: Build the plot ---
  fig <- plot_ly() %>%
    add_trace(
      data = marker_data,
      x = ~iterations,
      # Use the captured expression for the y-axis
      y = y_var_expr, 
      color = ~factor(id),
      type = 'scatter',
      mode = 'markers',
      legendgroup = ~factor(id)
    ) %>%
    add_trace(
      data = line_data,
      x = ~iterations,
      y = ~.fitted, # The fitted line data always uses '.fitted'
      color = ~factor(id),
      type = 'scatter',
      mode = 'lines',
      legendgroup = ~factor(id),
      showlegend = FALSE
    ) %>%
    # --- Step 3: Customize the layout with dynamic titles ---
    layout(
      title = paste(y_var_string, "vs. Iterations by ID"),
      xaxis = list(title = "Iterations"),
      yaxis = list(title = y_var_string),
      legend = list(title = list(text = "<b> ID </b>"))
    )
  
  # Return the final plot object
  return(fig)
}
#2010
plot_trends(marker_data = result1,
            y_var = sample10_percent)
#2016
plot_trends(marker_data = result1,
            y_var = sample16_percent)
#2020
plot_trends(marker_data = result1,
            y_var = sample20_percent)
# summary
## there are three locations (150,146,142) where the sample density is around 20% 
## 63 with only 4 locations require a full sample 
## The rest hover between 40 and 80, with 87,80 being very consistent across the years 
## possible group of  80, 77, 88, 79,146 
## possible group of 89 78 81 72 87 

