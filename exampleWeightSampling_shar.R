library(dplyr)

df4 <- data.frame(
  ID = 1:4, 
  area = c(10,20,30,40),
  TOF = runif(4, min = 0.02, max = 0.50)
)
df20 <- data.frame(
  ID = 1:20, 
  area = runif(20, min = 10, max = 40),
  TOF = runif(20, min = 0.02, max = 0.50)
)