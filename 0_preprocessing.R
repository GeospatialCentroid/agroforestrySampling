# run.R
# This script executes the pipeline defined in _targets.R

# 1. Make sure {targets} is installed
# install.packages("targets")

# 2. Run the pipeline
targets::tar_make(script = "targets_preprocessing.R")

# 3. (Optional) View the dependency graph
targets::tar_visnetwork(script = "targets_preprocessing.R")
