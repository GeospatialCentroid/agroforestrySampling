###
# download NAIP for a location then generate the SNIC objects for validation 
### 

# libraries
pacman::p_load(rstac,snic, sf, terra, dplyr, tmap, rlang, httr, tictoc, purrr, furrr)
tmap::tmap_mode("view")

source("functions/naipScrape.R")
source("functions/snicParameters.R")

# required inputs  --------------------------------------------------------
grid100 <- sf::st_read("data/derived/grids/grid100km_aea.gpkg")

# download and process NAIP image ----------------------------------------------------------------
## probably best to make this a function accepting either a point or a grid id for better integration into 
## the snic workflow 

# point example 
point <- c(-98.55915251753592,41.24362217062013)
aoi <- getAOI(grid100 = grid100, point = point)
qtm(aoi)

# grid id base approach 
aoi <- getAOI(grid100 = grid100, id = "1416-1-d-2-1" )
qtm(aoi)

# test for year
## probably unnecessary as for nebraska at least it seems to be 2012:2022 every even year
getNAIPYear(aoi = aoi)

# set year 
year <- "2020"
exportFolder <- "naip_grids_1km"
gridID <- aoi$id

# # download naip
downloadNAIP(aoi = aoi, year = year, exportFolder = exportFolder)

# files 
files <- list.files(
  exportFolder,
  pattern = paste0(year, "_id_", gridID),
  full.names = TRUE
) 

message(paste("Processing Grid:", gridID, "Year:", year))
out_path <- paste0(
  "data/derived/naipExports/naip_",
  year,
  "_id_",
  gridID,
  "_wgs84.tif"
)
# combined multiple naip file if there are present into a single 1km feature 
mergeAndExport(files = files, out_path =out_path, aoi = aoi )


# Generate the Snic imagery 
r1 <- terra::rast(out_path)

# generate seeds, list of dataframes with specifically spaced lat lon values 
seeds <- generate_scaled_seeds(r = r1)

# quick viz 
inspect_seed_density(r = r1, seed_list = seeds, seed_name ="s20" )

# generate snic objects 
process_segmentations(r = r1,
                      seed_list = seeds, 
                      output_dir = "data/derived/snicExports",
                      file_id = aoi$id )


