####
# goal : find the weighted average of area of a sample 
# 


library(dplyr)

df4 <- data.frame(
  ID = 1:4, 
  area = c(10,20,30,40),
  TOF = runif(4, min = 0.02, max = 0.50)
)
df20 <- data.frame(
  ID = 1:20, 
  area = runif(20, min = 10, max = 40),
  TOF = runif(4, min = 0.02, max = 0.50)
)


weightedSample <- function(df, sampleSize){ 
  
  
  # total area of population 
  totalAreaPop <- sum(df$area)
  
  # pull a sample 
  sample <- sample_n(tbl = df, size = sampleSize)
  
  # total area of sample 
  totalAreaSample <- sum(sample$area)
  # average area of sample 
  aveAreaSample <- totalAreaSample / nrow(sample)
  
  # proportionality factor 
  proFactor <- totalAreaPop / totalAreaSample
  
  # assign values 
  ## one of the main issue I was having in the past was using the unique area data, rather than the aveAreaSample
  ## in the calculation of the weigthed area measures 
  ## assumign that this is not required of the TOF because were using a independent measure that is not area 
  sample <- sample |>
    dplyr::mutate(
      relativeWeight = area/aveAreaSample, 
      weightedArea = aveAreaSample * relativeWeight * proFactor,
      weightTOF = TOF * relativeWeight * proFactor
    )
  
  # check the area measure 
  print(paste("Area population:", sum(df$area)))
  print(paste("Weighted area of sample", sum(sample$weightedArea)))
  sum(df$area) == sum(sample$weightedArea)
  
  # check the TOF calculations 
  df$prop <- df$area / sum(df$area)
  # this weighted mean calculation uses the % area of a specific region against the whole as the weight 
  weightMeanTOF <- weighted.mean(x = df$TOF, w = df$prop)
  
  # interestingly the average weighed area requires that you using the population of the denominator in the calculation 
  averageWeightedTOF <-  sum(sample$weightTOF) / nrow(df)
  print(paste("Weighted Mean TOF population:", weightMeanTOF))
  print(paste("Weighted Mean TOF sample", averageWeightedTOF))
  # exact match only when nrow sample == nrow population 
  print(weightMeanTOF == averageWeightedTOF)

}

# with four 
weightedSample(df = df4, sampleSize = 4)

# with 20 
weightedSample(df = df20, sampleSize = 8)




