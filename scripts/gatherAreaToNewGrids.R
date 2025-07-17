pacman::p_load(sf, dplyr, readr)


gridSF <- sf::st_read("data/derived/spatialFiles/grid10k.gpkg")

data <- readr::read_csv("data/derived/gridAreaSummaries/grids10k.csv")

# prep the data file 
d2 <- data |>
  dplyr::select(-Unique_ID)|>
  rowwise() %>%
  mutate(totalCells = sum(c_across("X0":"NA."),na.rm=TRUE)) |>
  ungroup() 


# group by ID and summarize select model with high totalcell for validation stats
summaryDF <- d2 |>
  group_by(ID) |>
  summarise(
    modelGrid = modelGrid[which.max(totalCells)],
    across(X0:NA., ~sum(., na.rm = TRUE)))|>
  rowwise() |>
  mutate(totalCells = sum(c_across("X0":"NA."),na.rm=TRUE)) |>
  ungroup() 

# join to spatial features 
s1 <- dplyr::left_join(x = gridSF, y = summaryDF, by = "ID")
# export 
sf::st_write(obj = s1, dsn = "data/products/newModelAreas/results10kGrid.gpkg")
