bundle_and_export <- function(grid_id, year, export_base = "data/ready_for_export") {
  
  # 1. Safety First: Store current WD and guarantee restoration
  original_wd <- getwd()
  on.exit(setwd(original_wd))
  
  # 2. Setup Directories
  if (!dir.exists(export_base)) dir.create(export_base, recursive = TRUE)
  
  # Convert export base to absolute path immediately
  abs_export_base <- normalizePath(export_base, mustWork = TRUE)
  
  # 3. Locate NAIP Files
  # We search specifically for the ID and Year in the filename
  naip_source_dir <- "data/derived/naipExports"
  naip_files_all  <- list.files(path = naip_source_dir, full.names = TRUE)
  
  # Filter for matches
  match_pattern <- paste0("naip_", year, "_id_", grid_id)
  naip_matches  <- naip_files_all[grepl(pattern = match_pattern, x = naip_files_all)]
  
  # 4. Locate SNIC Files
  snic_source_dir <- "data/derived/snicExports"
  # We convert this to absolute path now to ensure copy works later
  abs_snic_dir    <- normalizePath(snic_source_dir, mustWork = TRUE)
  
  snic_files_all  <- list.files(abs_snic_dir, full.names = TRUE)
  snic_matches    <- snic_files_all[grepl(grid_id, snic_files_all) & grepl(year, snic_files_all)]
  
  # 5. Create Staging Bundle
  bundle_name <- paste0("grid_", grid_id, "_", year, "_bundle")
  staging_dir <- file.path(abs_export_base, bundle_name)
  
  if (!dir.exists(staging_dir)) dir.create(staging_dir, recursive = TRUE)
  
  message(paste("Gathering files for Grid:", grid_id, "| Year:", year))
  
  # 6. Copy NAIP Files
  if (length(naip_matches) > 0) {
    # Normalize paths to ensure file.copy finds them from any working directory
    abs_naip_matches <- normalizePath(naip_matches, mustWork = FALSE)
    
    file.copy(from = abs_naip_matches, 
              to = file.path(staging_dir, basename(abs_naip_matches)), 
              overwrite = TRUE)
    message(paste("Copied", length(naip_matches), "NAIP file(s)."))
  } else {
    warning(paste("No NAIP files found for pattern:", match_pattern))
  }
  
  # 7. Copy SNIC Files
  if (length(snic_matches) > 0) {
    file.copy(from = snic_matches, 
              to = file.path(staging_dir, basename(snic_matches)), 
              overwrite = TRUE)
    message(paste("Copied", length(snic_matches), "SNIC files."))
  } else {
    warning(paste("No SNIC files found for Grid:", grid_id, "and Year:", year))
  }
}
