

### templating a function for running the sampling methods 


# geographic area of interest 
# 12 mile grids to understand what models will be required to be pulled in 
# sampling technique all from terra::spatSample
## random or regular 
## stratified or not 
## chess board sampling 
# number of samples 

## might want something the weights based on forest areas within the cell.
## would need to pass in the grid areas and join the forest cover values to each subgrid



## gathering points is one step 

## step two  
## selecting model grids to import 
## extracting values 
## determine presence/abscense by year 

## step three summaries the results 


pacman::p_load("terra", "dplyr")


# sampling function  ------------------------------------------------------

grids <- list.files("data/products/modelGrids",
                    pattern = ".gpkg",
                    full.names = TRUE)
g2 <- terra::vect(grids[grepl(pattern = "two_sq", x = grids)])
g10 <- terra::vect(grids[grepl(pattern = "2010", x = grids)])
g16 <- terra::vect(grids[grepl(pattern = "2016", x = grids)])
g20 <- terra::vect(grids[grepl(pattern = "2020", x = grids)])

# aoi's 
ecos <- terra::vect("data/derived/spatialFiles/us_eco_l3.gpkg")

samplePoints <- function(aoi, nSamples, random){
  # condition for sampling techinic 
  if(isTRUE(random)){
    method = "random"
  }else{
    method = "regular"
  }
  
  p1 <- terra::spatSample(x = aoi, 
                    method = method, 
                    size = nSamples)
}


p1 <- samplePoints(
  aoi = ecos[2, ],
  nSamples = 50,
  random = TRUE,
  chess = TRUE
)
## get a plot that show the results of the techniques 
aoi <- ecos[1,]
gridArea <- g10
nSamples <- 100 
random <- FALSE

# adjust this function so I can assign a proprotion sample size that attached to the object


samplePoints12m <- function(aoi, nSamples, random, gridArea, year){
  # crop and mask sub grid to the aoi 
  a1 <- gridArea |>
    terra::crop(aoi)
  # sample 
  p1 <- samplePoints(aoi = a1, 
                     nSamples = nSamples,
                     random = random)
  # plot results 
  terra::plot(a1)
  terra::plot(p1, add=TRUE,
              main = paste0("Random: ", random,
                            " N Sample: ", nSamples))
}

# Sampling on a the 12mile grid  ------------------------------------------
samplePoints12m(aoi = aoi,
                nSamples = 100, 
                random = FALSE,
                gridArea = gridArea,
                year = NA)






# Extract the model grids  ------------------------------------------------




