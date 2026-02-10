pacman::p_load(dplyr, terra)


# 2 mile grid  


unetImages <- list.files(
    "SNIC-Comparison/1km_test_grids",
    pattern = ".tif",
    full.names = TRUE
)


# comparisons to make 
## unet predcitions against RF predictions 
#3 - 1km_test_grids : _pred.tif

## unet predictions against the validated site imagery 

## SNIC classified against the unet _final.tif 


# Build relationship table
df_lookup <- data.frame(
  year = c("2020", "2020", "2020", "2016", "2016"),
  mile12_1d =c("X12-361","X12-300","X12-361","X12-661","X12-150"),
  mile2_id = c("17663", "10625", "24675", "30823", "6621"),
  km1_id = c(
    "1415-3-12-4-1",
    "1413-1-6-15-2",
    "1482-4-4-3-2",
    "1544-1-18-1-4",
    "1352-3-5-c-1"
  ),
  stringsAsFactors = FALSE
)

# pull in original classification imagery 
rfClass <- list.files("~/trueNAS/work/Agroforestry/data/products/models2016/fullImages", 
full.names=TRUE
) 
rf16 <- c("/home/dune/trueNAS/work/Agroforestry/data/products/models2016/fullImages/X12-150_fullUnMasked.tif",
 "/home/dune/trueNAS/work/Agroforestry/data/products/models2016/fullImages/X12-661_fullUnMasked.tif" )
r20 <- c( "/home/dune/trueNAS/work/Agroforestry/data/products/models2016/fullImages/X12-361_fullUnMasked.tif",
 "/home/dune/trueNAS/work/Agroforestry/data/products/models2016/fullImages/X12-300_fullUnMasked.tif", 
 "/home/dune/trueNAS/work/Agroforestry/data/products/models2016/fullImages/X12-661_fullUnMasked.tif")