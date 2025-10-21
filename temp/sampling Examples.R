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

# new sampling options 
us <- st_read("data/derived/us/lower48.gpkg")
us2 <- terra::vect(us) 
bb <- sf::st_convex_hull(us)

terra::plot(us2[us2$name == "Nebraska", ])


## 10k grids in MLRA 
g10 <- terra::vect("data/derived/spatialFiles/grid10k.gpkg")
mlra <- terra::vect("data/derived/spatialFiles/CONUS_MLRA_52_dissolved.gpkg")

t1 <- mlra[6,]

allG <- terra::relate(g10, t1, relation = "intersects")
# 3. Sbset your source vector
selected_vects_touch <- g10[allG[,1], ]



inG <- terra::intersect(g10, t1)
terra::plot(allG)

