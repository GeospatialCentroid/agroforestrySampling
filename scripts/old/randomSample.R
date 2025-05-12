### 
# sampling design idea 
# use the 12m grid to gather samples 
# test smaller areas within the sample 


pacman::p_load("terra", "dplyr", "readr", "sf", "tictoc", "tmap")





# Ecoregion layers  -------------------------------------------------------


# areas become the input value to the function 
vectPath <- "data/derived/spatialFiles/us_eco_l3.gpkg"
areaPath <- "data/derived/areaCounts/EPA_Level3"
prepAreasForSample <- function(vectPath, vectID,  areaPath){
  # aoi's 
  vect <- terra::vect(vectPath)
  # unique areas 
  areaNames <- unique(vect[,vectID])
  # area files 
  areaFiles <- list.files(path = areaPath,
                          full.names = TRUE)
  
  
}

# determine the total forested areas with the region of interest 
## using the central great plains as the reference areas 
cgp <- ecos[ecos$US_L3NAME == "Central Great Plains", ]
# area files 

cpgFiles <- areaFiles[grepl(pattern = "Central Great Plains", x = areaFiles)] |>
  readr::read_csv()

# select all grids directlu 
g10cpg <- g10[g10$Unique_ID %in% cpgFiles$Unique_ID, ] |>
  terra::crop(cgp)

# full area
fullArea <- terra::expanse(x = g10cpg, unit= "m") |> sum()
# percent tof
total10 <- (sum(cpgFiles$cells2010)/fullArea)*100
total16 <- (sum(cpgFiles$cells2016)/fullArea)*100
total20 <- (sum(cpgFiles$cells2020)/fullArea)*100

# remove any areas with less the full area 
df2 <- cpgFiles[cpgFiles$sameArea == TRUE, ]

# map of a selected grids 

fullAreas <- g10cpg[g10cpg$Unique_ID %in% df2$Unique_ID, ]
fullAreas_area <- terra::expanse(x = fullAreas, unit = "m") |> sum()

# plot 
terra::plot(cgp)
terra::plot(fullAreas, add = TRUE)

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

totalArea <- sq_m_to_ha(fullAreas_area)
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
    print(seed)
    # select i number of sites
    selection <- df4 |>
      slice_sample(n = i)
    # get the average trees per selection 
    totalA <- sum(selection$originalArea)
    totalTOF <- sum(selection$cells2010)
    # percent value 
    percentage <- (totalTOF/totalA)*100
    # store results
    if(seed == 1){
      results <- c(percentage)
    }else{
      results <- c(results, percentage)
    }
  }
  # summarize results 
  output$aveResults[i] <- mean(results)
  output$standardDev[i] <- sd(results)
}

# let rework this so that one seed is used for all iterations then we test a different seed

for(i in seeds){
  set.seed(i)
  # set storage dataframe
  output2 <- data.frame(sample = 1:nrow(df4),
                       aveResults = rep(0, nrow(df4)),
                       standardDev = rep(0, nrow(df4)),
                       seed = rep(i, nrow(df4)))
    for(j in 1:nrow(df4)){
      # select i number of sites
      selection <- df4 |>
        slice_sample(n = j)
      # get the average trees per selection 
      totalA <- sum(selection$originalArea)
      totalTOF <- sum(selection$cells2010)
      # percent value 
      percentage <- (totalTOF/totalA)*100
      # second percent calulation 
      percentageVals <- (selection$cells2010/selection$originalArea)*100
      
      
      # sort the results 
      output2$aveResults[j] <- mean(percentageVals)
      output2$standardDev[j] <- sd(percentageVals)
    }
  if(i == 1){
    results <- output2
  }else{
    results <- bind_rows(results, output2)
  }
}

# compare the results between the two options 





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

