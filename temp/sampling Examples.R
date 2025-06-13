pacman::p_load(sf, dplyr, tmap)
tmap_mode("view")

ecos <- st_read("data/derived/spatialFiles/us_eco_l3.gpkg")
e1 <- ecos[ecos$US_L3NAME == "Central Great Plains",]


# pull area specific info 
geoPaths <- getGeographicAreas(areaType = "eco")
columnID <- geoPaths$columnID

vect <- geoPaths$vect |> terra::vect() |> terra::aggregate(by = columnID)
files <- geoPaths$files
allNames <- unique(vect[,columnID]) |>
  as.data.frame() |> 
  pull()
## sub units wi