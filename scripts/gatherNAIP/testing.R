pacman::p_load(rstac, sf, terra, httr)


# Summary of workflow NAIP scape ---------------------------------------------------
## stand along script that can be used to gather NAIP imagery from a specific year
## using the lat lon or 1km grid id as a location reference

# elements
## 1. date check
## - using a point location, allows you to gather a vect of all dates at which NAIP is available.
## 2. Establish AOI
## - Starting either with a AOI or point, use the 100km grid features to generate the specific
## 1km grid element. This bounding box becomes the input for the data query
## 3. Download NAIP
## -
## 4. Process NAIP

# read in the 2 mile grid

## 1. Define Area of Interest (AOI) and Time
# Let's find imagery for a small area in Colorado
# You can create your own AOI using https://geojson.io/
aoi_lon_lat <- c(lon = -105.08, lat = 40.58) # Fort Collins, CO
year_of_interest <- "2021"

# Create a 1km bounding box around the point
# Convert point to a simple feature (sf) object, set CRS to WGS84
point_sf <- sf::st_sfc(sf::st_point(aoi_lon_lat), crs = "EPSG:4326")

# Buffer the point and get its bounding box
aoi_bbox <- sf::st_bbox(
  sf::st_transform(
    sf::st_buffer(
      sf::st_transform(point_sf, "EPSG:5070"), # Project to meters for buffering
      dist = 1000 # 1000-meter buffer
    ),
    "EPSG:4326" # Transform back to WGS84 for the query
  )
)

# Define the date-time range for the entire year
time_range <- paste0(
  year_of_interest,
  "-01-01T00:00:00Z/",
  year_of_interest,
  "-12-31T23:59:59Z"
)

## 2. Connect to the Planetary Computer STAC API
stac_endpoint <- "https://planetarycomputer.microsoft.com/api/stac/v1"

con <- rstac::stac(stac_endpoint)

## 3. Build and Send the Search Query
search_results <- con |>
  rstac::stac_search(
    collections = "naip", # The NAIP collection ID
    bbox = aoi_bbox, # Our area of interest
    datetime = time_range, # Our time range
    limit = 1 # Just get the first item found
  ) |>
  rstac::get_request() # Execute the search

# Check if any items were found
if (length(search_results$features) == 0) {
  stop("No NAIP imagery found for the specified AOI and year.")
}

## 4. Sign the Asset URLs
# Planetary Computer requires a "signed" URL for downloading.
# rstac has a built-in helper for this.
signed_items <- rstac::items_sign(
  search_results,
  rstac::sign_planetary_computer()
)

## 5. Get the Download URL for the Image
# The main NAIP image asset is named "image"
# We extract the signed URL (href) for this asset
image_url <- rstac::assets_url(signed_items, asset_names = "image")

# If there are multiple items, this will be a list. We'll just take the first.
download_url <- image_url[[1]]

print(paste("Found asset URL:", download_url))

## 6. Download the File Locally
# Define where to save the file
local_filepath <- "temp/naip_image.tif"

# Use httr::GET with write_disk to handle the large file download
# This is more robust than download.file() for large cloud assets
message("Downloading file to ", local_filepath, "...")
resp <- httr::GET(
  download_url,
  httr::write_disk(local_filepath, overwrite = TRUE),
  httr::progress() # Show a download progress bar
)

if (resp$status_code != 200) {
  stop("File download failed. Status code: ", resp$status_code)
} else {
  message("Download complete!")
}

## 7. Load and Plot the Downloaded Image
# You can now work with the local GeoTIFF file
local_naip_raster <- terra::rast(local_filepath)

# Print raster info
print(local_naip_raster)

# Plot the RGB bands
# NAIP is typically 4-band: Red, Green, Blue, Near-Infrared (NIR)
plotRGB(local_naip_raster, r = 1, g = 2, b = 3, stretch = "lin")
terra::plot(local_naip_raster)
