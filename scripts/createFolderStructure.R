# generate folder structure for the project
## add to this as new files are generated
folders <- c(
  "data",
  "data/derived",
  "data/derived/grids",
  "data/derived/us",
  "data/derived/mlra",
  "data/derived/mlraSummaries",
  "data/derived/aoitar",
  "data/raw",
  "data/raw/us",
  "data/raw/mlra",
  "data/raw/nlcd",
  "data/products"
)

for (i in folders) {
  if (!dir.exists(i)) {
    dir.create(i)
  } else {
    print(paste0(i, " exists"))
  }
}
