###
# over all goal; a standalone workflow that can be used to download NAIP imagery from mircosoft planetary computer for a set AOI 

# steps 
# 1. Generate a aoi from a point or block ID using the 100km grid feature 
# 2. download NAIP from the ms computer 
# 3. process imagery to requirements of the projects 








# libraries 
pacman::p_load(rstac, sf, terra, dplyr, tmap, rlang, httr)
tmap_mode("view")

# functions  --------------------------------------------------------------

# generate the a grid object  --------------------------------------------------
## currently duplicated from the preprocessingFunction.R
buildGrids <- function(extent_object, cell_size) {
  # transform to equal area
  ea <- sf::st_transform(extent_object, 5070)
  # generate grid
  grid <- sf::st_make_grid(
    x = ea,
    cellsize = cell_size
  )
  if ("id" %in% names(ea)) {
    ids <- paste0(ea$id[1], "-", as.hexmode(1:length(grid)))
  } else {
    ids = as.hexmode(1:length(grid))
  }
  # generate ID
  gridID <- sf::st_sf(
    id = ids,
    geomentry = grid
  )
  # export
  return(gridID)
}

# generate subgrids objects
buildSubGrids <- function(grids, cell_size, aoi) {
  # generate sub grids
  subGrids <- grids |>
    dplyr::group_split(id) |>
    purrr::map(.f = buildGrids, cell_size = cell_size) |>
    dplyr::bind_rows()
  # apply the filter again --
  subGrids <- subGrids[aoi, ]
  
  return(subGrids)
}

getAOI <- function(grid100, point = FALSE, id = FALSE){
  # condition for setting input type to test 
  if(!isFALSE(point)){
    message("Grabing aoi based on lat lon value ")
    # generate a point object and convert to albert equal area
    pointFeature <- sf::st_point(point) |>
      sf::st_sfc(crs = "EPSG:4326") |> 
      st_transform(crs = "EPSG:5070")
    
    # intersect with 100km grid 
    gid <- grid100[pointFeature,]|> 
      as.data.frame()|>
      dplyr::pull("id")
    
    # 100k grid
    g1 <- grid100[grid100$id == gid, ]
    
    ### it be worth building this into a specific function. 
    # to get the specific id for the grids I need to generate the full set ( 50,10,2,1) 
    # filter and generate to new area 50k 
    t1 <- buildSubGrids(grids = g1, cell_size = 50000, aoi = g1)[pointFeature, ]
    # filter and generate to new area 10k 
    t2 <- buildSubGrids(grids = t1, cell_size = 10000, aoi = t1)[pointFeature, ]
    # filter and generate to new area 2k 
    t3 <- buildSubGrids(grids = t2, cell_size = 2000, aoi = t2)[pointFeature, ]
    # generate 1km grids 
    t4 <- buildSubGrids(grids = t3, cell_size = 1000, aoi = t3)[pointFeature, ]
    # export the 1km grid feature 
    return(t4)
  }
  
  if(!isFALSE(id)){
    message("Grabing aoi based on ID")
    # parse out the id to the specific geographies 
    feat_names <- c("id100", "id50", "id10", "id2", "id1")
    # parse out string and apply names 
    ids <- id |> 
      stringr::str_split( pattern = "-") |>
      unlist()
    # construct the ids for specific selection 
    id100 <- ids[1]
    id50 <- paste(id100, ids[2], sep = "-")
    id10 <- paste(id50, ids[3], sep = "-")
    id2 <- paste(id10, ids[4], sep = "-")
    id1 <- paste(id2, ids[5], sep = "-")
    
    # select 100k grid 
    g1 <- grid100 |>
      dplyr::filter(id == id100)
    # build the 50k grids 
    t1 <- buildSubGrids(grids = g1, cell_size = 50000, aoi = g1) |>
      dplyr::filter(id == id50)
    # build the 10k grids 
    t2 <- buildSubGrids(grids = t1, cell_size = 10000, aoi = t1) |>
      dplyr::filter(id == id10)
    # build the 50k grids 
    t3<- buildSubGrids(grids = t2, cell_size = 2000, aoi = t2) |>
      dplyr::filter(id == id2)
    # build the 50k grids 
    t4 <- buildSubGrids(grids = t3, cell_size = 1000, aoi = t3) |>
      dplyr::filter(id == id1)
    #export feature  
    return(t4)
  }
  # error test if nothing was added to the function
  if(isFALSE(point) | isFALSE(id)){
    message("No input provided. Please provide a value for either the point or id object")
  }
}

getNAIPYear <- function(aoi){
  # prep aoi object 
  bbox <- aoi |>
    st_transform(crs = "EPSG:4326") |>
    sf::st_bbox()
  # Connect to STAC API
  stac_endpoint <- "https://planetarycomputer.microsoft.com/api/stac/v1"
  con <- rstac::stac(stac_endpoint)
  # see what comes ups 
  message("pulled results from the specific aoi.")
  search_results <- con |>
    rstac::stac_search(
      collections = "naip",
      bbox = bbox,
      limit = 200 # A high limit to get all records
    ) |>
    rstac::get_request() # Execute the search
  if (length(search_results$features) == 0) {
    stop("No NAIP imagery found for the specified AOI.")
  }
  
  # pull dates 
  all_datetimes <- rstac::items_datetime(search_results)
  # pull specific year 
  all_years_str <- substr(all_datetimes, 1, 4)
  # return only unique values 
  available_years <- sort(unique(all_years_str))
  ## 5. Show Results
  message("Query complete.")
  message("Naip is available at the following years")
  print(available_years)
}

downloadNAIP <- function(aoi, year, exportFolder){
  # prep aoi object 
  bbox <- aoi |>
    st_transform(crs = "EPSG:4326") |>
    sf::st_bbox()
  # check to see if the export path existing 
  if(!dir.exists(exportFolder)){
    message("create a director at the export path")
    dir.create(exportFolder)
  }
  
  # Define the date-time range for the entire year
  time_range <- paste0(
    year, "-01-01T00:00:00Z/",
    year, "-12-31T23:59:59Z"
  )
  
  ## Connect to the Planetary Computer 
  stac_endpoint <- "https://planetarycomputer.microsoft.com/api/stac/v1"
  con <- rstac::stac(stac_endpoint)
  
  ## 3. Build and Send the Search Query
  search_results <- con |>
    rstac::stac_search(
      collections = "naip", 
      bbox = bbox,         
      datetime = time_range,    
      limit = 10                 # 10:: shouldn't be more than 1, but lets see... 
    ) |>
    rstac::get_request() # Execute the search
  
  # Check if any items were found
  if (length(search_results$features) == 0) {
    stop("No NAIP imagery found for the specified AOI and year.")
  }
  # Planetary Computer requires a "signed" URL for downloading.
  signed_items <- rstac::items_sign(
    search_results,
    rstac::sign_planetary_computer()
  )
  
  ## Get the Download URL for the Image
  # The main NAIP image asset is named "image"
  # We extract the signed URL (href) for this asset
  image_url <- rstac::assets_url(signed_items, asset_names = "image")
  
  # get a base export path 
  exportPath <- paste0(exportFolder, "/naip_",year,"_id_",aoi$id)
  
  # If there are multiple items, this will be a list. We'll just take the first.
  if(length(image_url) > 1) {
    message("multiple images are being returned")
    # generate an index for multiple files 
    index <- 1:length(image_url)
    # download and export individually 
    for(i in index){
      ex2 <- paste0(exportPath, "_",i,".tif")
      message("Downloading file to ", ex2, "...")
      resp <- httr::GET(
        image_url[[i]],
        httr::write_disk(ex2, overwrite = TRUE),
        httr::progress() # Show a download progress bar
      )
    }
  }else{
    # single feature so simplified naming convention 
    ex2 <- paste0(exportPath,".tif")
    # grab element 
    download_url <- image_url[[1]]
    message("Downloading file to ", ex2, "...")
    resp <- httr::GET(
      download_url,
      httr::write_disk(ex2, overwrite = TRUE),
      httr::progress() # Show a download progress bar
    )
  }
  # conditional testing to show failure 
  if (resp$status_code != 200) {
    stop("File download failed. Status code: ", resp$status_code)
  } else {
    message("Download complete!")
  }
}

standardizeNAIP <- function(importPath, exportPath){
  # read in 
  r1 <- terra::rast(importPath ) 
  # assign names 
  names(r1) <- c("red","green","blue","nir")
  # test visualization 
  terra::plotRGB(r1, r = "red", g = "green", b = "blue", stretch = "lin")
  
  # create a template raster at 1 m 
  message("generating a 1m template raster")
  r_template_1m <- terra::rast(
    extent = ext(r1),
    crs = crs(r1),
    resolution = 1 # 1 meter
  )
  
  # Resample the Original to the template
  message("resampling the image to 1m ")
  r_new <- terra::resample(
    r1,
    r_template_1m,
    method = "bilinear" # "bilinear" for continuous data, "near" for categorical
  )
  
  # reproject to wgs 84 
  message("projecting to wgs84")
  r_new_84 <-  r_new |>
    terra::project(
      "EPSG:4326",
      method = "bilinear"
    )
  
  # export 
  message("exporting image to the export path ")
  terra::writeRaster(x = r_new_84, filename = exportPath)
}

# required inputs  --------------------------------------------------------
grid100 <- sf::st_read("data/derived/grids/grid100km_aea.gpkg")


# testing 
point <- c(lon = -97.59819236732471, lat = 42.236962949995366)



# workflow ----------------------------------------------------------------
aoi <- getAOI(grid100 = grid100, point = point)
qtm(aoi)

# test for year 
getNAIPYear(aoi = aoi)

# download naip 
downloadNAIP(aoi = aoi, year = 2021, exportFolder = "temp")

r1 <- terra::rast("temp/naip_2021_id_1998-3-12-4-4.tif")

#standard 
standardizeNAIP(importPath = "temp/naip_2021_id_1998-3-12-4-4.tif",
                exportPath = "naip_2021_id_1998-3-12-4-4_wgs84.tif")


# download naip for the specific aoi for a specific year 
year <- "2021"
exportFolder <- "temp"


# test what years are available at the aoi 
getNAIPYear(aoi = aoi )

# download area 
downloadNAIP(aoi = aoi, year = "2021", exportFolder = "temp")


importPath <- "temp/naip_2021_id_1344-4-12-f-4.tif"









