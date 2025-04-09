### 
# sampling design idea 
# use the 12m grid to gather samples 
# test smaller areas within the sample 


pacman::p_load("terra", "dplyr", "readr", "sf", "tictoc", "tmap")


grids <- list.files("data/products/modelGrids",
                    pattern = ".gpkg",
                    full.names = TRUE)
# g2 <- terra::vect(grids[grepl(pattern = "two_sq", x = grids)])
g10 <- terra::vect(grids[grepl(pattern = "2010", x = grids)])
g16 <- terra::vect(grids[grepl(pattern = "2016", x = grids)])
g20 <- terra::vect(grids[grepl(pattern = "2020", x = grids)])

# aoi's 
ecos <- terra::vect("data/derived/spatialFiles/us_eco_l3.gpkg")

# determine the total forested areas with the region of interest 
## using the central great plains as the reference areas 
cgp <- ecos[ecos$US_L3NAME == "Central Great Plains", ]
# area files 
areaFiles <- list.files(path = "data/derived/areaCounts/EPA_Level3",
                        full.names = TRUE)
cpgFiles <- areaFiles[grepl(pattern = "Central Great Plains", x = areaFiles)] |>
  readr::read_csv()

# full area
fullArea <- terra::expanse(x = cgp, unit = "m")
# percent tof
total10 <- (sum(cpgFiles$cells2010)/fullArea)*100
total16 <- (sum(cpgFiles$cells2016)/fullArea)*100
total20 <- (sum(cpgFiles$cells2020)/fullArea)*100

# remove any areas with less the full area 
df2 <- cpgFiles[cpgFiles$sameArea == TRUE, ]

# calculate the 
# Function to convert square kilometers to hectares
sq_km_to_ha <- function(sq_km) {
  hectares <- sq_km * 100  # 1 square kilometer = 100 hectares
  return(hectares)
}
# Function to convert square meters to hectares
sq_m_to_ha <- function(sq_meters) {
  hectares <- sq_meters / 10000  # 1 hectare = 10,000 square meters
  return(hectares)
}

# covert the area measures to standardized unit hectarce
df3 <- df2 |>
  mutate(originalArea = sq_km_to_ha(df2$originalArea))|>
  mutate(across(c(cells2010,cells2016,cells2020), sq_m_to_ha))

totalArea <- sum(df3$originalArea) 
total10 <- (sum(df3$cells2010)/totalArea)*100
total16 <- (sum(df3$cells2016)/totalArea)*100
total20 <- (sum(df3$cells2020)/totalArea)*100
  
# calculate the percent area of each grid per year
percentArea <- function(num, dem){
  val <- (num/dem)*100
}
df4 <- df3 |>
  mutate(tof10 = percentArea(cells2010, originalArea),
         tof16 = percentArea(cells2016, originalArea),
         tof20 = percentArea(cells2020, originalArea))
  
# from here we randomly selecting areas calculating the average tof per for the year 
# Do this 10 times and take the average of the iterations 
# then increase the sample 
seeds <- 1:10
year <- 2010 
threshold <- total10
margin <- 0.1
low <- threshold - (threshold * margin)
high <- threshold + (threshold * margin)

output <- data.frame(sample = 1:nrow(df4), aveResults = rep(0, nrow(df4)), standardDev = rep(0, nrow(df4)))

for(i in 1:nrow(df4)){
  for(seed in seeds){
    set.seed(seed)
    # select i number of sites
    selection <- df4 |>
      slice_sample(n = i)
    # get the average trees per selection 
    tof_10 <- mean(selection$tof10)
    # store results
    if(i == 1){
      results <- c(tof_10)
    }else{
      results <- c(results, tof_10)
    }
  }
  # summarize results 
  output$aveResults[i] <- mean(results)
  output$standardDev[i] <- sd(results)
}



# old from 04 01 meeting  -------------------------------------------------




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


pacman::p_load("terra", "dplyr", "readr", "tictoc", "tmap")


# sampling function  ------------------------------------------------------

grids <- list.files("data/products/modelGrids",
                    pattern = ".gpkg",
                    full.names = TRUE)
# g2 <- terra::vect(grids[grepl(pattern = "two_sq", x = grids)])
g10 <- terra::vect(grids[grepl(pattern = "2010", x = grids)])
g16 <- terra::vect(grids[grepl(pattern = "2016", x = grids)])
g20 <- terra::vect(grids[grepl(pattern = "2020", x = grids)])

# aoi's 
ecos <- terra::vect("data/derived/spatialFiles/us_eco_l3.gpkg")

# determine the total forested areas with the region of interest 
## using the central great plains as the reference areas 
cgp <- ecos[ecos$US_L3NAME == "Central Great Plains", ]
# area files 
areaFiles <- list.files(path = "data/derived/areaCounts/EPA_Level3",
                        full.names = TRUE)
cpgFiles <- areaFiles[grepl(pattern = "Central Great Plains", x = areaFiles)] |>
  readr::read_csv()
# full area
fullArea <- terra::expanse(x = cgp, unit = "m")
# percent tof
total10 <- (sum(cpgFiles$cells2010)/fullArea)*100
total16 <- (sum(cpgFiles$cells2016)/fullArea)*100
total20 <- (sum(cpgFiles$cells2020)/fullArea)*100



# functiuons  -------------------------------------------------------------

## itorative method to determine the number of locations required for a percentile match 
fullSample <- function(df, year, threshold, margin, seed){
  # set seed 
  set.seed(seed)
  # set max attempt 
  max_attempts <- 100 # Limit the number of attempts to avoid infinite loops
  # parameters for first loop 
  n = 1000
  attempt <- 0
  
  # find acceptable range 
  low <- threshold - (threshold * margin)
  high <- threshold + (threshold * margin)
  
  
  # set the tof values of interest 
  if(year == 2010){
    vals <- c(1,4,6,9)
  }
  if(year == 2016){
    vals <- c(3,4,8,9)
  }
  if(year == 2020){
    vals <- c(5,6,8,9)
  }
  mean <- 0
  while(TRUE){
    # sample ten itorations at N and average 
    for(i in 1:20){
      selection <- df |>
        # slice_sample(n = round(n))|>
        filter(ChangeOverTime %in% vals)|>
        nrow()
      if(i == 1){
        average <- selection
      }else{
        average <- c(average, selection)
      }
    }
    # average selection 
    mean <- mean(average)/n
    print(mean)
    # print(mean)
    if(mean >= low && mean <= high){
      return(round(n))
      stop()
    }else{
      attempt <- attempt + 1
      if(mean > high){
        n = n/1.5
      }else{
        n = n * 2
      }
    }  
    print(n)
  }
}

# spatial sample random or systematic 
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




# sampling method 1 : random draw each time  ------------------------------



# sampling method 2 : single large draw  -------------------------------
## generate a random sample of points across the region, 
## exatract values from those points 

# crop the 12m grid to aoi 
aoi2 <- terra::crop(g10, cgp)

ranSample <- samplePoints(aoi = aoi2, nSamples = 1000000, random = TRUE)
tmap_mode("view")
# qtm(sf::st_as_sf(ranSample))

# get a list of AOI from the random points 
areas <- unique(ranSample$Unique_ID)
# for over areas, read in raster and extract points from that area 
for(i in seq_along(areas)){
  # grid 
  gridName <- areas[i]
  print(gridName)
  # 
  file <- paste0("~/trueNAS/work/Agroforestry/data/products/changeOverTime/",
                 gridName,"_changeOverTime_2.tif")
  if(file.exists(file)){
    # read in raster
    r1 <- terra::rast(file)
    # filter selection and extract 
    p1 <- ranSample[ranSample$Unique_ID == gridName, ]
    
    # extracted values 
    vals <- terra::extract(x = r1[[1]], y = p1)
    # save as df 
    df2 <- as.data.frame(p1) |>
      dplyr::mutate(ChangeOverTime = vals$ChangeOverTime)|>
      dplyr::select(Unique_ID, ChangeOverTime)
    # just bind for now 
    if(i == 1){
      df <- df2
    }else{
      df <- bind_rows(df, df2)
    }
  }else{
    
  }
}
# export these results 
## need a more comprehesive way for naming 
write_csv(df, paste0("data/derived/spatialSampling/ecoRegion_CentralGreatPlains_random_10000000.csv"))

# df <- read_csv(paste0("data/derived/spatialSampling/ecoRegion_CentralGreatPlains_random_1000000.csv"))

## from here I can use the same sampling method from method 3 
## test sample at a few seeds 
seeds <- 1:10
results <- data.frame(year = c(2010,2016,2020), sample = NA)
margin <- 0.01

## loop for calculating the aver number needed 
for(i in 1:3){
  year <- results$year[i]
  if(year == 2010){
    threshold <- total10
  }
  if(year == 2016){
    threshold <- total16
  }
  if(year == 2020){
    threshold <- total20
  }
  for(j in seeds){
    val <- fullSample(df = df,
                      year = year,
                      threshold = threshold,
                      margin = margin,
                      seed = j)
    if(j == 1){
      result <- val
    }else{
      result <- c(result, val)
    }
    print(result)
  }
  results[i,2] <- round(mean(result))
}


# sampling method 3 : full area ----------------------------
grids <- cpgFiles$Unique_ID

# pull a grid and convert to point 
## using a grid that we know is fully inside of the sample area X12-213

r1 <- terra::rast("~/trueNAS/work/Agroforestry/data/products/changeOverTime/X12-213_changeOverTime_2.tif")
tic()
points <- as.data.frame(terra::values(r1$ChangeOverTime))
toc()
# full grid ~ 7.5 secs 
### would need to export this file but lets work with it for now 


# looking at the frequency of measure in this area 
freq <- points %>%
  count(ChangeOverTime) %>%
  mutate(proportion = n / sum(n))


df <- points
year <- 2016
threshold <- total16
margin <- 0.10
seed <- 1234



## test sample at a few seeds 
seeds <- 1:10
results <- data.frame(year = c(2010,2016,2020), sample = NA)

for(i in 1:3){
  year <- results$year[i]
  for(j in seeds){
    val <- fullSample(df = df,
                      year = year,
                      threshold = threshold,
                      margin = margin,
                      seed = j)
    if(j == 1){
      result <- val
    }else{
      result <- c(result, val)
    }
  }
  results[i,2] <- round(mean(result))
}







# original Effort ---------------------------------------------------------






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
aoi <- ecos[2,]
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
  nSamples = 100,
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
  mutate(count2010 = round(250 * proportion2010),0)

# sample points per grid base on the count 
df2 <- df[df$count2010 >0, ] |>
  dplyr::select("Unique_ID", "cells2010", "total_2010","proportion2010", "count2010")
View(df2)

df3 <- df2[,c("Unique_ID","count2010")]

aoi <- ecos[2,]
gridArea <- g10 |>
  terra::crop(aoi)

gsf <- sf::st_as_sf(gridArea)

for(i in 1:nrow(df2)){
  # select spatial object from side ID 
  g1 <- gridArea[gridArea$Unique_ID == df2$Unique_ID[i], ]
  
  d1 <- samplePoints(aoi = g1, 
               nSamples = df2$count2010[i],
               random = TRUE)
  if(i == 1){
    d2 <- d1
  }else{
    d2 <- rbind(d2,d1)
  }
}
# bind data to sf object 
gs <- terra::merge(x = gridArea, y = df2, by = "Unique_ID")

terra::plot(gridArea)
terra::plot(d2, add = TRUE)
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
samplePointsMaps(aoi = aoi,
                nSamples = 100, 
                random = FALSE,
                gridArea = gridArea,
                year = NA)


