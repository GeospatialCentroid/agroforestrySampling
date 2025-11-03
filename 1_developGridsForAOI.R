# 1_developGridsForAOI.R
# This script generates the various gridded objects for the AOI of interest

# aoi name
aoiName <- "Nebraska"

# Run the pipeline
targets::tar_make(script = "targets_sampleGrids.R")

# View the dependency graph not required
targets::tar_visnetwork(script = "targets_sampleGrids.R")
