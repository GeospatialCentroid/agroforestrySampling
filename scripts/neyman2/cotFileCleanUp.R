# --- CLEANUP SCRIPT -----------------------------------------------------------

# 1. Define the directory
target_dir <- "data/raw/changeOverTime"

# 2. List ALL files in that directory
all_files <- list.files(target_dir, full.names = TRUE)

# 3. Identify files to KEEP (Ending in "_2.tif")
# We use regex to find files ending ($) in _2.tif
files_to_keep <- grep("_2\\.tif$", all_files, value = TRUE)

# 4. Identify files to DELETE (The difference between All and Keep)
files_to_delete <- setdiff(all_files, files_to_keep)

# --- SAFETY CHECK (DRY RUN) ---------------------------------------------------
message(paste("Found", length(all_files), "total files."))
message(paste("Identified", length(files_to_keep), "files to KEEP."))
message(paste("Identified", length(files_to_delete), "files to DELETE."))

if (length(files_to_delete) > 0) {
  message("\nFiles slated for deletion (First 10):")
  print(head(files_to_delete, 10))

  # --- EXECUTE DELETION -------------------------------------------------------
  # UNCOMMENT the line below when you are 100% sure!

  file.remove(files_to_delete)

  # message("Deletion complete.")
} else {
  message("No files found to delete.")
}
