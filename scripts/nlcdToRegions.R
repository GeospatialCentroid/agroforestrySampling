pacman::p_load(terra, dplyr,sf, readr, purrr, googledrive, tidyr, stringr, plotly,
               tictoc)

source("functions/samplingWorkflowFunctions.R")
# grab grid cells 
grids <- getGrids(gridSize = 10)
# read in NLCD data 
nlcdFiles <- list.files("data/derived/nlcdData",
                        pattern = "_AEA",
                        full.names = TRUE,
                        all.files = TRUE)|>
  terra::rast()

mlra <- TRUE 

# working with MLRA and LRR so set some parameters based on that 
if(isTRUE(mlra)){
  # extract area counts for each grid of the unique MLRA areas 
  files <- pullAreaFiles(featName = "MLRA", size = 10)
  export1 <- "data/derived/nlcdSummaryAreas/allClasses_summaryMLRA.csv"
  export2 <- "data/derived/nlcdSummaryAreas/mlra_"
  nameCol <- "MLRA_ID"
  pattern1 <- "mlra_"
}else{
  files <- pullAreaFiles(featName = "LRR", size = 10)
  export1 <- "data/derived/nlcdSummaryAreas/allClasses_summaryLRR.csv"
  export2 <- "data/derived/nlcdSummaryAreas/lrr_"
  nameCol <- "LRR_NAME"
  pattern1 <- "lrr_"
}

index <- 1:nrow(files$spatial)
name <- files$name

if(!file.exists(export1)){
  # for each MLRA grid 
  for(i in index){
    # the MLRA boundary 
    g1 <- files$spatial[i]
    # get the name object 
    name <- g1[[nameCol]][1] |> pull()
    print(name)
    # grids within the boundry 
    #project to AEA for nlcd data 
    g2 <- prepGrids(grids = grids, subGeo = g1)|>
      terra::project(crs(nlcdFiles))
    # crop 
    n1 <- terra::crop(nlcdFiles, g2) |>
      terra::mask(g2)
    # total area 
    ## this is being a memory hog... just use the vector file 
    # area <- terra::expanse(n1[[1]], unit = "km")[,"area"]
    # area <- sum(terra::expanse(g2, unit = "km"), na.rm = TRUE)

    ## direct extraction of values 
    for(j in 1:3){
      vals <- terra::values(n1[[j]]) 
      vals <- vals[!is.na(vals)]
      if( j == 1){
        nCells <- length(vals)
        # areaPerCell <- area/nCells
        e1 <- data.frame("Annual_NLCD_LndCov_2010"= rep(NA, nCells),
                         "Annual_NLCD_LndCov_2016" = rep(NA, nCells),
                         "Annual_NLCD_LndCov_2020" = rep(NA, nCells))
      }
      e1[,j] <- vals
    }
    # format the data 
    e2 <- e1 |>
      tidyr::pivot_longer(
        cols = starts_with("Annual_NLCD"),
        names_to = "Year",
        values_to = "Land_Cover_Class"
      )|>
      mutate(
        Year = str_extract(Year, "\\d{4}")
      )|>
      dplyr::count(Year, Land_Cover_Class, name = "Count")
    # export the data 
    write_csv(
      x = e2, 
      file = paste0(
        export2,name,".csv"
      )
    )
  }
  
  # summarize the regional features 
  files2 <- list.files(path = "data/derived/nlcdSummaryAreas",
                       pattern = pattern1,
                       full.names = TRUE) 
  
  
  
  for(i in seq_along(files2)){
    feat <- files2[i]
    name <- stringr::str_remove(basename(feat), pattern = ".csv")
    # read and process to full areas
    t1 <- read_csv(feat)
    # trying with an assumed area measure of a pixel, where Count is the 
    # number of individual pixels 
    t2 <- dplyr::mutate(t1,
                        calc_area = (Count * 900)/1000000,
                        areaName = name
    )
    
    if(i ==1){
      results <- t2
    }else{
      results <- dplyr::bind_rows(results,t2)
    }
  }
  
  
  # assign the classes 
  results2 <- results |>
    dplyr::mutate(
      class = case_when(
        Land_Cover_Class == 11 ~ "Open Water",
        Land_Cover_Class == 12 ~ "Perennial Ice/Snow",
        Land_Cover_Class == 21 ~ "Developed, Open Space",
        Land_Cover_Class == 22 ~ "Developed, Low Intensity",
        Land_Cover_Class == 23 ~ "Developed, Medium Intensity",
        Land_Cover_Class == 24 ~ "Developed High Intensity",
        Land_Cover_Class == 31 ~ "Barren Land",
        Land_Cover_Class == 41 ~ "Deciduous Forest",
        Land_Cover_Class == 42 ~ "Evergreen Forest",
        Land_Cover_Class == 43 ~ "Mixed Forest",
        Land_Cover_Class == 51 ~ "Dwarf Scrub",
        Land_Cover_Class == 52 ~ "Shrub/Scrub",
        Land_Cover_Class == 71 ~ "Grassland/Herbaceous",
        Land_Cover_Class == 72 ~ "Sedge/Herbaceous",
        Land_Cover_Class == 73 ~ "Lichens",
        Land_Cover_Class == 74 ~ "Moss",
        Land_Cover_Class == 81 ~ "Pasture/Hay",
        Land_Cover_Class == 82 ~ "Cultivated Crops",
        Land_Cover_Class == 90 ~ "Woody Wetlands",
        Land_Cover_Class == 95 ~ "Emergent Herbaceous Wetlands"
      )
    )
  #export 
  write_csv(x = results2, file = export1)
  
}


# Group my categories from NLCD  ------------------------------------------
# water, develop, barren, forest, shrubland, herbaceous, cul;tivated, wetlands 
if(isTRUE(mlra)){
  file <- read_csv("data/derived/nlcdSummaryAreas/allClasses_summaryMLRA.csv")
  export3 <- "data/derived/nlcdSummaryAreas/largeClass_summaryMLRA.csv"
}else{
  file <- read_csv("data/derived/nlcdSummaryAreas/allClasses_summaryLRR.csv")
  export3 <- "data/derived/nlcdSummaryAreas/largeClass_summaryLRR.csv"
}

# assign larger class 
d1 <- file |>
  dplyr::mutate(lcGroup = case_when(
    Land_Cover_Class %in% c(11,12) ~ "Water",
    Land_Cover_Class %in% c(21,22,23,24) ~ "Developed",
    Land_Cover_Class %in% c(31) ~ "Barren",
    Land_Cover_Class %in% c(41,42,43) ~ "Forest",
    Land_Cover_Class %in% c(51,52) ~ "Shrubland",
    Land_Cover_Class %in% c(71,72,73,74) ~ "Herbaceous",
    Land_Cover_Class %in% c(81,82) ~ "Planted/Cultivated",
    Land_Cover_Class %in% c(90,95) ~ "Wetlands",
      )
    )|>
  group_by(Year,areaName, lcGroup) |>
  summarize(
    # group_area = sum(total_area ),
    group_area = sum(calc_area ),
    .groups = 'drop'
  ) |>
  dplyr::select(
    Year,areaName,  lcGroup, group_area
  )



# get the total area of all MLRA objects 
areas <- files$spatial |> 
  terra::project(nlcdFiles)
areas$areaKM <- terra::expanse(files$spatial, unit = "km")


areasKM <- as.data.frame(areas)[ , c(`nameCol`,"areaKM") ] 
areasKM$areaName  <- paste0(pattern1, areasKM[[nameCol]])                                              
# join to group summaries 
d2 <- dplyr::left_join(d1, areasKM, by = "areaName") |>
  dplyr::mutate(
    percentArea = (group_area/areaKM)*100
  ) |> 
  dplyr::select(-nameCol)

# export larger group data 
write_csv(d2, file =export3 )

# # test 
# t1 <- d2 |>
#   group_by(Year, areaName)|>
#   summarize(sum = sum(percentArea))
# 


# generate some plots -----------------------------------------------------
grabMeanSamples <- function(data){
  data |>  
    group_by(id)|>
    summarise(
      # Keep the first value found for these columns within each group
      totalAreas = first(totalAreas),
      tof = first(tof),
      # Calculate the average for these columns, removing NAs
      average_sample = mean(sample, na.rm = TRUE),
      average_samplePercentage = mean(samplePercentage, na.rm = TRUE))|>
    dplyr::mutate(
      average_TOF_perArea = ( (tof/100) /totalAreas) * 100
    )
}


# Generating plots for the area sample method  ----------------------------

if(isTRUE(mlra)){
  ## read in the aggregates data values 
  agg <- read_csv("data/derived/samplingXTesting/aggregated/mlra80_iterations.csv")
  ag10 <-  read_csv("data/derived/samplingXTesting/aggregated/mlra80_10.csv") |> grabMeanSamples()
  ag16 <-  read_csv("data/derived/samplingXTesting/aggregated/mlra80_16.csv") |> grabMeanSamples()
  ag20 <-  read_csv("data/derived/samplingXTesting/aggregated/mlra80_20.csv") |> grabMeanSamples()
  
}else{
  ## read in the aggregates data values 
  agg <- read_csv("data/derived/samplingXTesting/aggregated/lrr80_iterations.csv")
  ag10 <-  read_csv("data/derived/samplingXTesting/aggregated/lrr80_10.csv") |> grabMeanSamples()
  ag16 <-  read_csv("data/derived/samplingXTesting/aggregated/lrr80_16.csv") |> grabMeanSamples()
  ag20 <-  read_csv("data/derived/samplingXTesting/aggregated/lrr80_20.csv") |> grabMeanSamples()
  
}



producePlots <- function(nlcdData, sampleData, year){
  # subset to a specific year 
  df_filtered <- nlcdData |>
    filter(
      Year == year
    )
  # assign the mlra column to match nlcd data 
  sampleData$mlra <- paste0("mlra_", sampleData$id)
  
  # Create the interactive grouped bar chart
  fig <- plot_ly(
    data = df_filtered,
    x = ~areaName ,
    y = ~percentArea,
    color = ~lcGroup,
    type = 'bar',
    barmode = 'group' # This groups the bars side-by-side
  )|>
    add_trace(
      data = sampleData,
      x = ~mlra,
      y = ~average_samplePercentage, # Use the column from your new data
      type = 'scatter',
      mode = 'markers',
      name = "Sampled Percentage",   # This name will appear in the legend
      marker = list(color = 'grey', size = 6, symbol = 'diamond'), # Customize the dot
      inherit = FALSE          # Prevents inheriting properties from the bar chart
    ) |>
    layout(
      title = paste0("NLCD class areas and Sample Percentage - ", year),
      xaxis = list(title = "MLRA", categoryorder = "category ascending"),
      yaxis = list(title = "Percent Area (%)", range = c(0, 100)), # Adjust range for text
      legend = list(title = list(text = "<b> Land Cover Group </b>"))
    )
}

p10 <- producePlots(nlcdData = d2, sampleData = ag10,year = "2010")
p10
p16 <- producePlots(nlcdData = d2, sampleData = ag16,year = "2016")
p16
p20 <- producePlots(nlcdData = d2, sampleData = ag20,year = "2020")
p20



