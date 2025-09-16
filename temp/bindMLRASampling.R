pacman::p_load(dplyr, readr)

# gather the records for all sample iterations
f1 <- list.files("data/derived/samplingXTesting/",
                 full.names = TRUE)
for(i in seq_along(f1)){
  feat <- f1[i]
  name1 <- paste0(sub("\\.csv$", "", basename(feat)),"_16")

  if(i ==1){
    d1 <- read_csv(feat)|> 
      dplyr::select(id,
                    totalAreas,
                    wTOF16, 
                    averageTOF16,
                    sample16)
    names(d1) <- c("id","totalArea","wTOF16","averageTOF16", `name1`)
    results <- d1
  }else{
    d1 <- read_csv(feat) |> 
      dplyr::select(id,
                    sample16)
    names(d1) <- c("id", `name1`)
    # join 
    results <- dplyr::left_join(results, d1)
  }
}

# sort results 
results <- results |>
  dplyr::select(
    id, totalArea,  wTOF16,
    mlra80_5_16, "mlra80_10_16", mlra80_20_16,mlra80_50_16,mlra80_100_16, mlra80_200_16, mlra80_500_16, mlra80_1000_16)
# export 
write_csv(results, "temp/variableItorations_2016_MLRA.csv")
