pacman::p_load(terra, dplyr,sf, readr, purrr, googledrive, tidyr, stringr )

# extract area counts for each grid of the unique MLRA areas 
files <- pullAreaFiles(featName = "MLRA", size = 10)
index <- 1:nrow(files$spatial)
name <- files$name
grids <- getGrids(gridSize = 10)
# read in NLCD data 
nlcdFiles <- list.files("data/derived/nlcdData",
                        pattern = "_AEA",
                        full.names = TRUE,
                        all.files = TRUE)|>
  terra::rast()


# for each MLRA grid 
for(i in index){
  # the MLRA boundary 
  g1 <- files$spatial[i]
  # get the name object 
  name <- g1$MLRA_ID[1]
  # grids within the boundry 
  #project to AEA for nlcd data 
  g2 <- prepGrids(grids = grids, subGeo = g1)|>
    terra::project(crs(nlcdFiles))
  # crop 
  n1 <- terra::crop(nlcdFiles, g2) |>
    terra::mask(g2)
  # total area 
  area <- terra::expanse(n1[[1]], unit = "km")[,"area"]
  nCells <- terra::ncell(n1[[1]])
  areaPerCell <- area/nCells
  # extract the values 
  e1 <- terra::extract(x = nlcdFiles, y = g2)
  e2 <- e1 |>
    tidyr::pivot_longer(
      cols = starts_with("Annual_NLCD"),
      names_to = "Year",
      values_to = "Land_Cover_Class"
      )|>
    mutate(
      Year = str_extract(Year, "\\d{4}")
      )|>
    dplyr::count(ID, Year, Land_Cover_Class, name = "Count")|>
    dplyr::mutate(
      area = Count * areaPerCell
    )
  # export the data 
  write_csv(
    x = e2, 
    file = paste0(
      "data/derived/nlcdSummaryAreas/mlra_",name,".csv"
    )
  )
}

# summarize the regional features 
files2 <- list.files(path = "data/derived/nlcdSummaryAreas",
                     pattern = "mlra_",
                     full.names = TRUE) 
for(i in seq_along(files2)){
  feat <- files2[i]
  name <- stringr::str_remove(basename(feat), pattern = ".csv")
  # read and process to full areas
  t1 <- read_csv(feat) |>
    group_by(Year, Land_Cover_Class) |>
    summarize(
      total_area = sum(area),
      .groups = 'drop'
    ) |>
    mutate(
      mlra = name
    )
  if(i ==1){
    results <- t1
  }else{
    results <- dplyr::bind_rows(results,t1)
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
write_csv(x = results2, file = "data/derived/nlcdSummaryAreas/allClasses_summaryMLRA.csv")


# Group my categories from NLCD  ------------------------------------------
# water, develop, barren, forest, shrubland, herbaceous, cul;tivated, wetlands 
file <- read_csv("data/derived/nlcdSummaryAreas/allClasses_summaryMLRA.csv")

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
  group_by(Year,mlra, lcGroup) |>
  summarize(
    group_area = sum(total_area ),
    .groups = 'drop'
  ) |>
  dplyr::select(
    Year,mlra,  lcGroup, group_area
  )
# get the total area of all MLRA objects 
mlras <- files$spatial
mlras$areaKM <- terra::expanse(files$spatial, unit = "km")
mlrasArea <- mlras |>
  as.data.frame()|>
  dplyr::mutate(
    mlra = paste0("mlra_", as.character(MLRA_ID))
  )|>
  dplyr::select(
    mlra, areaKM
  )
# join to group summaries 
d2 <- dplyr::left_join(d1, mlrasArea, by = "mlra") |>
  dplyr::mutate(
    percentArea = (group_area/areaKM)*100
  )

# test 
t1 <- d2 |>
  group_by(Year, mlra)|>
  summarize(sum = sum(group_area))
