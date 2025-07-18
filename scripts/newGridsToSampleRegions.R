# this method is going to limited to 
pacman::p_load(dplyr, terra,sf,readr,tidyr, furrr)

future::plan(strategy = "multicore", workers = 12)
# for each region 
## for each sub region 
## intersect all 12mile model grids 
## intersect all 10km grids 
## read in a 12 mile grid and crop 
## crop 10km feature to 12 mile area 
## extract values to the new areas 
## repeat 
### sumamrize by grid ID 
### export 



# grid feature 
grid10 <- terra::vect("data/products/newModelAreas/results10kGrid.gpkg")
grid10$originalArea <- round(terra::expanse(x = grid10, unit="km"),2)
grid12 <- terra::vect("data/products/modelGrids/modelGrids_2010.gpkg")
grid12$originalArea <- round(terra::expanse(x = grid12, unit="km"),2)
# summarize data by subset regions ------------------------------------------
regions <- list.files("data/derived/spatialFiles",
                      full.names = TRUE)
# read in objects and standard id column 
eco <- regions[grepl(pattern = "us_eco", regions)] |> terra::vect()
eco$ID <- paste0("ECO_",eco$US_L3NAME)
mlra <- regions[grepl(pattern = "MLRA", regions)] |> terra::vect()
mlra$ID <- paste0("MLRA_", mlra$MLRA_ID)
lrr <- regions[grepl(pattern = "LRR", regions)] |> terra::vect()
lrr$ID <- paste0("LRR_", lrr$LRR_NAME)
koppen <- regions[grepl(pattern = "Koppen", regions)] |> terra::vect()
koppen$ID <- paste0("koppen_", koppen$ClimCode)
# change over time files 
cots <- list.files("~/trueNAS/work/Agroforestry/data/products/changeOverTime",
                   pattern = "_2.tif",
                   full.names = TRUE)

summarizeSubAreas <- function(index,g12,g10,cots,name){
  g1 <- g12[index,]
  print(paste0(index," out of ", nrow(g12)))
  # name 
  rastname <- g1$Unique_ID
  # intersect 
  subAreas <- terra::intersect(g10, g1)
  # read in raster object 
  r1 <- cots[grepl(pattern = paste0(rastname,"_change"), x = cots)] |> terra::rast()
  r1 <- r1$ChangeOverTime
  if(nrow(subAreas)>0){
    for(unit in 1:nrow(subAreas)){
      u1 <- subAreas[unit, 1:11]
      exportPath2 <- paste0("data/derived/areaCounts/10k/pixels_",u1$ID,"_",name,".csv")
      if(!file.exists(exportPath2)){
        #crop and mask 
        r2 <- try(terra::crop(r1,u1))
        if(class(r2)!="try-error"){
          r2 <- terra::mask(r2, u1)
          # summarize 
          vals <- terra::values(r2)|>
            as.data.frame()|>
            dplyr::count(ChangeOverTime, name = "counts")|>
            dplyr::filter(!is.nan(ChangeOverTime))|>
            tidyr::pivot_wider(names_from = ChangeOverTime, values_from = counts)
          u1[,3:11] <- vals[1,]
          # export path 
          write_csv(x = as.data.frame(u1), file = exportPath2)
        }
      }
    }
  }
}


for(region in list(eco,mlra, lrr, koppen)){
  print(region)
  # for each region 
  subRegions <- unique(region$ID)
  ## for each sub region 
  for(i in subRegions){
    s1 <- region[region$ID == i, ] 
    # grab name for export paths 
    name <- s1$ID[1]
    # aggregate to single feature
    s1 <- s1 |>
      terra::aggregate() |> 
      terra::makeValid()
    
    ## intersect all 10km grids 
    g10 <- terra::intersect(grid10, s1)[,1:12]
    names(g10) <- names(grid10)
    g10$newArea <- round(terra::expanse(x = g10, unit="km"),2)
    g10$diff <- g10$originalArea == g10$newArea
    
    # spit out based on change in areas 
    noChange <- g10[g10$diff == TRUE, ]
    g10 <- g10[g10$diff == FALSE, ]
    
    # exprot the data for nochange 
    for(j in 1:nrow(noChange)){
      feat <- noChange[j, ]  
      id <- feat$ID
      # export path 
      exportPath1 <- paste0("data/derived/areaCounts/10k/pixels_",id,"_",name,".csv")
      if(!file.exists(exportPath1)){
        write_csv(x = as.data.frame(feat)[,1:11], file = exportPath1)
      }
    }
    
    ## intersect all 10km grids 
    g12 <- terra::intersect(grid12, s1)[,c(1,4)]
    g12$newArea <- round(terra::expanse(x = g12, unit="km"),2)
    g12$diff <- g12$originalArea == g12$newArea
    
    # spit out based on change in areas 
    g12 <- g12[g12$diff == FALSE, ]
    
    index <- 1:nrow(g12)
    
    furrr::future_map(.x = index, .f = summarizeSubAreas,
                      g12 = g12,
                      g10 = g10,
                      cots = cots,
                      name = name)
    
    
    ### for loop for troubleshooting 
    # summarizeSubAreas(g12 = g12, index = index, g10 = g10, cots = cots,name = name)
    # for(j in 1:nrow(g12) ){ # nrow(g12)
    #   g1 <- g12[j,]
    #   print(paste0(j," out of ", nrow(g12)))
    #   # name
    #   name <- g1$Unique_ID
    #   # intersect
    #   subAreas <- terra::intersect(g10, g1)
    #   # read in raster object
    #   r1 <- cots[grepl(pattern = paste0(name,"_change"), x = cots)] |> terra::rast()
    #   r1 <- r1$ChangeOverTime
    #   if(nrow(subAreas)>0){
    #     for(unit in 1:nrow(subAreas)){
    #       u1 <- subAreas[unit, 1:11]
    #       #crop and mask
    #       r2 <- try(terra::crop(r1,u1))
    #       if(class(r2)!="try-error"){
    #         r2 <- terra::mask(r2, u1)
    #         # summarize
    #         vals <- terra::values(r2)|>
    #           as.data.frame()|>
    #           dplyr::count(ChangeOverTime, name = "counts")|>
    #           dplyr::filter(!is.nan(ChangeOverTime))|>
    #           tidyr::pivot_wider(names_from = ChangeOverTime, values_from = counts)
    #         u1[,3:11] <- vals[1,]
    #         # export path
    #         exportPath2 <- paste0("data/derived/areaCounts/10k/pixels_",u1$ID,"_",name,".csv")
    #         write_csv(x = as.data.frame(u1), file = exportPath2)
    #       }
    #     }
    #   }
    # }
  }
}




