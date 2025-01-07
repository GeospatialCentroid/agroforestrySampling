## reclass to get a 2016 and 2020 value 
getYearMap <- function(raster, year){
  if(year == 2010){
    # define the replacement values 
    m <- rbind(c(0, 0),
               c(1, 1),
               c(3, 0),
               c(4, 1),
               c(5, 0),
               c(6, 1),
               c(8, 0),
               c(9, 1))
    r2 <- raster$ChangeOverTime |> 
      terra::classify(m,others=NA)
  }
  if(year == 2016){
    # define the replacement values 
    m <- rbind(c(0, 0),
               c(1, 0),
               c(3, 1),
               c(4, 0),
               c(5, 0),
               c(6, 0),
               c(8, 0),
               c(9, 0))
    r2 <- raster$ChangeOverTime |> 
      terra::classify(m,others=NA)
  }
  if(year == 2020){
    # define the replacement values 
    m <- rbind(c(0, 0),
               c(1, 0),
               c(3, 0),
               c(4, 0),
               c(5, 1),
               c(6, 0),
               c(8, 0),
               c(9, 0))
    r2 <- raster$ChangeOverTime |> 
      terra::classify(m,others=NA)
  }
  
  return(r2)
}