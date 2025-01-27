# Overview 

Second phase of the larger Agroforestry project. 

Aim: Using the establish classification of trees outside of forests in Nebraska to test and validate a variety of 
sample designs that allow use to estimate expected trees outside of forest from a subset of the full coverage classifcation. 



# Folder and file descriptions 

## data  -- this is going to be ignored for github, so contact dan for access to current information.
*derived* : 
  - areaCounts : folder for storing the results of the calculations of total area of tree outside of forest for the stratification geographies of interest
  - all NLCD classes : summary of the area of all NLCD classes per state 
  - forestArea : summary of total forest area from CDL datasets per state 
  - state areas : summary of total area per state 
  - urban areas : summary of total urban area per state
*products* :
 - modelGrids : folder containing the model grid reference files
 - nlcdMask : gpkg files that are used as forest area masks 
 - urbanMask : us census places file that is used for urban areas mask 
*raw* : 
 - spatialAreaFiles : gpkg files of the established area in which spatial stratification will take place 
 - ecoregions -- epa lvl 3
 - koppen climate zones 
 - LRR 
 - MLRA 



## figure 
 - storing outputs that are shared elsewhere. Trying using date stamps in file names 
 
## functions 
- reclassByYear.R : generic function for processing the change over time data format to a specific year. Heavy processing effect to run this

## scripts 
- summarizeTheAreaResults.R : Take output from the calculateForestAreaPerRegion and produce a map and bar chart 
- calculateForestAreaPerRegion.R : workflow for producing counts of trees outside of forest at a specific geography



