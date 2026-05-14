library(haven)
raw_dir <- file.path("k:", "master", "R_learning", "JPSP_2016", "data", "raw")
out_dir <- file.path("k:", "master", "R_learning", "JPSP_2016", "data", "processed")
sav_files <- list.files(raw_dir, pattern = "\\.sav$", full.names = TRUE)
cat("Found", length(sav_files), "SAV files\n")
for (f in sav_files) {
  d <- read_sav(f)
  base <- sub("\\.sav$", "", basename(f))

  # RDS — preserve all SPSS metadata
  rds_out <- file.path(out_dir, paste0(base, ".rds"))
  saveRDS(d, rds_out)

  # CSV — universal readable format (strip haven labels)
  csv_out <- file.path(out_dir, paste0(base, ".csv"))
  d_csv <- as.data.frame(lapply(d, function(x) {
    if (inherits(x, "haven_labelled")) as.character(haven::as_factor(x)) else x
  }))
  write.csv(d_csv, csv_out, row.names = FALSE, fileEncoding = "UTF-8")

  cat("Converted:", basename(f), "->", paste0(base, ".rds"),
      "+", paste0(base, ".csv"),
      "|", ncol(d), "vars,", nrow(d), "rows\n")
}
cat("Done.\n")
