####
# goal : find the weighted average of area of a sample 
# 


library(dplyr)



df20 <- data.frame(
  ID = 1:20, 
  area = runif(20, min = 10, max = 40),
  TOF = runif(20, min = 0.02, max = 0.50)
)

df <- df20
sampleSize <- 10

# simplified version that I'm using in the workflow  

# total area of population 
## used in both the total TOF calculation and the sample TOF 
totalAreaPop <- sum(df$area)

# area-weighted mean TOF
tofRatio <- weighted.mean(x = df$TOF, w = df$area)
#TOF area population 
tofTotal <- tofRatio * totalAreaPop

# generate the sample 
sample <- sample_n(df, size = sampleSize)
# ratio estimator from the sample
sampleRatio <- weighted.mean(x = sample$TOF, w = sample$area)
# TOF area estimate 
tofEstimate <- sampleRatio * totalAreaPop



# worked example that gabriel provided 
weightedSample <- function(df, sampleSize){ 
  # total area of population 
  totalAreaPop <- sum(df$area)
  
  # population truth (area-weighted mean TOF)
  pop_ratio <- sum(df$area * df$TOF) / totalAreaPop
  tofRation <- weighted.mean(x = df$TOF, w = df$area)
  
  #TOF area
  TOF_total <- (weighted.mean(x = df$TOF, w = df$area))*totalAreaPop
  
  # pull a sample 
  sample <- sample_n(df, size = sampleSize)
  
  ## total area estimator check - this is not needed
  profactor <- totalAreaPop/sum(sample$area)
  area_check <- sum(sample$area*profactor)
  
  # ratio estimator from the sample
  ratio_est <- sum(sample$area * sample$TOF) / sum(sample$area)
  
  
  # estimated total TOF area in the population
  TOF_total_est <- ratio_est * totalAreaPop
  
  # print results
  cat("Population total area:", totalAreaPop, "\n")
  cat("Population TOF proportion:", pop_ratio, "\n")
  cat("Population total TOF:", TOF_total, "\n")
  cat("Population total area from sample:", area_check, "\n")
  cat("Sample ratio estimator (TOF proportion):", ratio_est, "\n")
  cat("Estimated total TOF area:", TOF_total_est, "\n\n")
}
weightedSample(df20, 10)

