pacman::p_load(terra,targets,purrr,dplyr,sf)


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
gridIDs <- unique(relationTable$g12_id)
gridID <- gridIDs[1]
sel_MLRA <- mlraSG
grid_12 <- grid12
tofAreaToSubGrid <- function(gridID, relationTable, grid_12, sel_MLRA ){
  # export
  export <- paste0("temp/subGridTOF/gridID_",gridID,".csv")
  
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
    sg3$area <- terra::expanse(sg3)
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








vals <- purrr::map(
  .x = c("X12-660", "X12-660"),
  .f = ~ tofAreaToSubGrid(
    gridID = .x,  # .x is the current value (660, then 662)
    relationTable = relationTable,
    grid_12 = grid12,
    sel_MLRA = mlraSG
  )
)

# summarize and aggregate
output <- vals |>
  dplyr::bind_rows()|>
  group_by(id) %>%
  dplyr::summarize(
    # Sum these columns
    gridArea = sum(gridArea, na.rm = TRUE),
    tof2010_sum = sum(tof2010_sum, na.rm = TRUE),
    tof2016_sum = sum(tof2016_sum, na.rm = TRUE),
    tof2020_sum = sum(tof2020_sum, na.rm = TRUE),
    # Keep the first value of this one
    ID = first(ID)
  ) %>%
  ungroup()

for(i in unique(ex1$g12_id)){
  # filter sub grids 
  sg2 <- ex1 |>
    dplyr::filter(g12_id == i)
  # select all sub grid features 
  sg3 <- selMLRA[sg2$subGridID, ]
  rm(sg2)
  # generate an ID column to join back too 
  sg3$ID <- 1:nrow(sg3)
  gridsDF <- as.data.frame(sg3)
  # grab rasters 
  p1 <- cotPaths[grepl(pattern = paste0("/X12-", i,"_"), cotPaths)]
  # read in features 
  # generate and crop raster 
  r1 <- terra::rast(p1) |>
    terra::crop(sg3)|>
    terra::mask(sg3)
  rm(p1)
  
  names(r1) <- c("tof2010","tof2016","tof2020")
  # remove all zeros 
  r1[r1 ==0, ] <- NA
  # Generate single area feature 
  area <- terra::cellSize(r1[[1]], unit = "km", mask = FALSE) 
  # multiple the TOF features by area 
  r2 <- r1 *area
  rm(r1, area)
  
  # extract TOF to subgrids this is the intesive part 
  tofExt <- terra::extract(x = r2, y = sg3)
  rm(r2, sg3)
  gc()
  
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
    )
  rm(tofExt)
  gc()
  # join to grids 
  results <- gridsDF |>
    dplyr::left_join(tofFormat, by ="ID")
  # export for tonight 
write_csv(results, paste0("temp/subGridTOF/"))
  
  return(results)
}





# iterate over the sub grids 
sub_ids <- 1:nrow(selMLRA)
for(i in sub_ids){
  # select the spatial object 
  sub <- selMLRA[i,]
  # filter the spatial relationship data 
  ex2 <- ex1 |>
    dplyr::filter(subGridID == i)
  # use the g12_id to grab all exptect images 
  rasts <- 
  if(nrow(ex2)>1){
    for(j in ex2$g12_id){
      
    }
  }else{
    p1 <- cotPaths[grepl(pattern = paste0("/X12-", ex2$g12_id,"_"), cotPaths)]
    # generate and crop raster 
    r1 <- terra::rast(p1) |>
      terra::crop(sub)|>
      terra::mask(sub)
    
    # convert the rast to area measures
    rast_cell_areas <- terra::cellSize(r1, unit = "km") |>
      terra::mask(r1)
    # extract the areas
    extractAreas <- terra::extract(
      rast_cell_areas,
      sub
    )
    names(extractAreas) <-c("ID", paste0(names(r1), "_area"))
    
    rm(rast_cell_areas)
    # extract values from raster object
    extractVals <- terra::extract(
      r1,
      sub
    ) |>
      dplyr::mutate(area = extractAreas$area)
    rm(rastVals)
    # join results 
    
    # summarize by id
    
    # determine the
    class_counts <- extractVals |>
      dplyr::group_by(ID, class) |>
      dplyr::summarise(
        totalCount = n(),
        valArea = sum(area, na.rm = TRUE),
        .groups = "drop"
      )
    rm(extractVals)
  }

}










# get the area of features
grid_feature <- terra::vect(grid_feature)
grid_feature$gridArea <- terra::expanse(grid_feature, unit = "km")
# temp id for join data back too
grid_feature$ID <- 1:nrow(grid_feature)
# coditon for the class of the raster_layer object
if (class(raster_layer) == "character") {
  raster_layer <- terra::rast(raster_layer)
}

# spatial filter the raster data
rastVals <- raster_layer |>
  terra::crop(grid_feature) |>
  terra::mask(grid_feature)
rm(raster_layer)
# convert the rast to area measures
rast_cell_areas <- terra::cellSize(rastVals, unit = "km") |>
  terra::mask(rastVals)
# extract the areas
extractAreas <- terra::extract(
  rast_cell_areas,
  grid_feature
)
rm(rast_cell_areas)
# extract values from raster object
extractVals <- terra::extract(
  rastVals,
  grid_feature
) |>
  dplyr::mutate(area = extractAreas$area)
rm(rastVals)
names(extractVals) <- c("ID", "class", "area")
# summarize by id

# determine the
class_counts <- extractVals |>
  dplyr::group_by(ID, class) |>
  dplyr::summarise(
    totalCount = n(),
    valArea = sum(area, na.rm = TRUE),
    .groups = "drop"
  )
rm(extractVals)
gc()

# add the area of the id
wide_df <- class_counts |>
  pivot_wider(
    id_cols = ID,
    names_from = class,
    values_from = c(totalCount, valArea),
    values_fill = 0 # Optional: replaces NA with 0
  )
# join this back to the mlra File and export
export <- grid_feature |>
  as.data.frame() |>
  dplyr::left_join(wide_df, by = "ID") %>%
  dplyr::mutate(across(
    .cols = starts_with("valArea_"),
    .fns = ~ (.x / gridArea) * 100,
    .names = gsub("valArea", "percentArea", "{.col}")
  )) |>
  rowwise() |>
  mutate(
    totalPercentArea = sum(c_across(starts_with("valArea_")))
  ) |>
  ungroup()


