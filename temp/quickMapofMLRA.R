pacman::p_load(leaflet, sf)


mlra <- sf::st_read("data/derived/spatialFiles/CONUS_MLRA_52_dissolved.gpkg")


# leaflet map 
m <- leaflet()|>
  addTiles() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  # Method 1: Using fillColor = "transparent"
  addPolygons(
    data = mlra,
    fillColor = "transparent",
    color = "#F8FEF260", # Outline color
    weight = 2,
    popup = ~MLRA_ID,
    label = ~MLRA_ID,
    labelOptions = labelOptions(
      noHide = TRUE, # This makes the label always visible
      direction = 'center', # Tries to place label in the center
      textOnly = TRUE, # Removes the white box and border
      style = list(
        "color" = "yellow",
        "font-weight" = "bold"
      )
    )
  ) 
m
