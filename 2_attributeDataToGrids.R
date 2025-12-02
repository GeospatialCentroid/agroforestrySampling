# 1_developGridsForAOI.R
# This script generates the various gridded objects for the AOI of interest

# Run the pipeline
targets::tar_make(script = "targets_attributeGrids.R")

# View the dependency graph not required
targets::tar_visnetwork(script = "targets_attributeGrids.R")
