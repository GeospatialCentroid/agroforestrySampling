

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
  random = FALSE
)

terra::plot(ecos[2,])
terra::plot(p1, add=TRUE)


## get a plot that show the results of the techniques 
aoi <- ecos[1,]
gridArea <- g10
nSamples <- 100 
random <- FALSE

# adjust this function so I can assign a proprotion sample size that attached to the obje


sampleGrids <- function(aoi, nSamples, random, gridArea){
  # crop and mask sub grid to the aoi 
  a1 <- gridArea |>
    terra::crop(aoi) 
  # sample 
  p1 <- samplePoints(aoi = a1, 
                     nSamples = nSamples,
                     random = random)
  # get grid ID 
  return(p1)
}

# stratified random sample  -----------------------------------------------
points <- sampleGrids(
  aoi = aoi,
  nSamples = 500,
  random = TRUE,
  gridArea = gridArea
)
# get the forest count information 
files <- list.files(path = "data/derived/areaCounts/EPA_Level3",
                    pattern = ".csv",
                    full.names = TRUE)
## select features with name from aoi object 
aoiID <- aoi$US_L3NAME[1]
feats <- files[grepl(pattern = aoiID, x = files)]
## read in features to single dataframe
df <- readr::read_csv(feats)

## propotional assing sample 

# Calculate proportions
df <- df %>%
  mutate(total_2010 = sum(cells2010)) %>%
  mutate(proportion2010 = cells2010 / total_2010)|>
  mutate(count2010 = round(500 * proportion2010),0)

# sample points per grid base on the count 
df2 <- df[df$count2010 >0, ]
vals <- 
for(i in 1:nrow(df2)){
  # select spatial object from side ID 
  g1 <- g10[g10$Unique_ID == df2$Unique_ID[i], ]
  
  d1 <- samplePoints(aoi = g1, 
               nSamples = df2$count2010[i],
               random = TRUE)
  if(i == 1){
    d2 <- d1
  }else{
    add(d2) <- d1
  }
}


# Extract the model grids  ------------------------------------------------




samplePointsMaps <- function(aoi, nSamples, random, gridArea, year){
  # crop and mask sub grid to the aoi 
  a1 <- gridArea |>
    terra::crop(aoi) 
  a1$weight <- 
    # sample 
    p1 <- samplePoints(aoi = a1, 
                       nSamples = nSamples,
                       random = random,
    )
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


