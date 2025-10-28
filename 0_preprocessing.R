# 0_preprocessing.R 
# This script executes the pipeline defined in targets_preprocessing.R 
# these are data sets that do not require any specific AOI 


# Run the pipeline
targets::tar_make(script = "targets_preprocessing.R")

# View the dependency graph not required 
targets::tar_visnetwork(script = "targets_preprocessing.R")
  