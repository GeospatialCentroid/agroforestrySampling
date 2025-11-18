pacman::p_load(terra,targets,purrr,dplyr,sf,readr)


# workflow 
## For all subgrid areas with a single MLRA
## test the intesection with the 12 mile grid 
## Group into cases - single 12mile or more than one 
## Itorate over single 12 mile 
### read in raster, crop and count pixels 
### for more then 2
### itorate over each grid, read in imagery, crop then combine or just extract 


# mlra sub grids 
mlraSG <- tar_read(mlraGrid_att)

# 12 mile grid 
grid12 <- terra::vect(tar_read(grid12))

# TOF files 
cotPaths <- tar_read(cotPaths)

defineGridRelationships <- function(mlraID, mlra_subGrids, grid_12, cot_paths){
  # select grids of interest
  selMLRA <- terra::vect(mlra_subGrids[mlra_subGrids$MLRA_ID == mlraID,])
  # assign area to selMLRA 
  selMLRA$gridArea <-terra::expanse(selMLRA, unit = "km")

  # attribute all grid ids to unique 1km grids 
  ex1 <- terra::relate(x = grid_12, y = selMLRA,relation = "intersects", pairs = TRUE ) |>
    as.data.frame() 
  # use the row indexs to construct a dataframe with proper reference values 
  # grid12 ids 
  base_ids <- grid_12$Unique_ID[ex1[, 1]]
  # subGrid ids 
  source_ids <- selMLRA$id[ex1[, 2]]
  
  # This table maps every single intersection
  id_lookup_table <- data.frame(
    g12_id = base_ids,
    subGridID = source_ids
  ) |>
    dplyr::mutate(MLRA_ID = mlraID)
  # export the object 
  return(id_lookup_table)
}

dg2 <- purrr::map(
  .x = unique(mlraSG$MLRA_ID),
  .f = ~defineGridRelationships(mlraID = .x, 
                                mlra_subGrids = mlraSG, 
                                grid_12 = grid12, 
                                cot_paths = cotPaths)
)

relationTable <- bind_rows(dg2)
# gridIDs <- unique(relationTable$g12_id)
# gridID <- gridIDs[1]
# sel_MLRA <- mlraSG
# grid_12 <- grid12
tofAreaToSubGrid <- function(gridID, relationTable, grid_12, sel_MLRA ){
  #convert to vector 
  # sel_MLRA <- terra::vect(sel_MLRA)
  # filter sub grids 
  sg2 <- relationTable |>
    dplyr::filter(g12_id == gridID)
  # select all sub grid features 
  sg3 <- sel_MLRA[sel_MLRA$id %in% sg2$subGridID, ]
  
  # export
  export <- paste0("temp/subGridTOF/gridID_",gridID,"_mlra_",sg2$MLRA_ID[1],".csv")
  
  if(!file.exists(export)){
    # convert to terra 
    # grid_12 <- terra::vect(grid_12)
    sel_MLRA <- terra::vect(sel_MLRA)
    # filter sub grids 
    sg2 <- relationTable |>
      dplyr::filter(g12_id == gridID)
    # select all sub grid features 
    sg3 <- sel_MLRA[sel_MLRA$id %in% sg2$subGridID, ]
    
    
    # add an area column 
    sg3$gridArea <- terra::expanse(sg3, unit = "km")
    # generate an ID column to join back too 
    sg3$ID <- 1:nrow(sg3)
    
    gridsDF <- as.data.frame(sg3)
    # grab rasters 
    p1 <- cotPaths[grepl(pattern =gridID, cotPaths)]
    # read in features 
    # generate and crop raster 
    r1 <- terra::rast(p1) |>
      terra::crop(sg3)|>
      terra::mask(sg3)
    names(r1) <- c("tof2010","tof2016","tof2020")
    # remove all zeros 
    r1[r1 ==0, ] <- NA
    # Generate single area feature 
    area <- terra::cellSize(r1[[1]], unit = "km", mask = FALSE) 
    # multiple the TOF features by area 
    r2 <- r1 *area
    
    # extract TOF to subgrids this is the intesive part 
    tofExt <- terra::extract(x = r2, y = sg3)
    # format 
    tofFormat <- tofExt |>
      group_by(ID) %>%
      summarize(
        # The 'across' function here applies the sum function
        # one column at a time. It does not mix them.
        across(
          c("tof2010","tof2016","tof2020"), 
          ~ sum(.x, na.rm = TRUE),
          # This creates names like "X12-660_2010_sum"
          .names = "{.col}_sum"
        )
      ) |>
      dplyr::ungroup()
    
    # join to grids 
    results <- gridsDF |>
      dplyr::left_join(tofFormat, by ="ID")|>
      dplyr::select(id, gridArea, tof2010_sum, tof2016_sum, tof2020_sum)
    # export for tonight 
    write_csv(results, export)
  }
}





for(i in c(72,77)){
  print(i)
  #filtering to single mlra 
  relationTable1 <- relationTable[relationTable$MLRA_ID == i, ]
  # run process
  vals <- purrr::map(
    .x = relationTable1$g12_id,
    .f = ~ tofAreaToSubGrid(
      gridID = .x,  # .x is the current value (660, then 662)
      relationTable = relationTable1,
      grid_12 = grid12,
      sel_MLRA = mlraSG
    )
  )
  
  
}






