# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Mon Aug 10 2026
# ------------------------------------------------- #

# Build fast-to-load copies of the package datasets.
#
# The package ships the GBD2021 tables as compressed .rda files in data/,
# which take ~22s to deserialize at app startup (bzip2 decompression is very
# slow on large tables). This script produces lean .rds copies in
# inst/extdata/ that the Shiny app loads instead:
#   * data.table format -- the app already converts to data.table at startup,
#     so this drops the conversion step entirely;
#   * character columns factorized -- region/sex shrink the cod table from
#     112MB to ~82MB and speed up filtering;
#   * gzip compression -- roughly 3x faster to decompress than bzip2 with a
#     comparable compression ratio.
#
# Result: COD 9.6s -> 0.45s, SDG 8.3s -> ~0.5s, LT 2.1s -> ~0.15s.
#
# Reproduce with: Rscript data-raw/build_fast_data.R
# The public .rda datasets in data/ are left untouched.

suppressPackageStartupMessages(library(data.table))

data_dir <- "data"
out_dir  <- file.path("inst", "extdata")
dir.create(out_dir, showWarnings = FALSE)

# Factorize the character columns (region/sex) of a dataset and return a
# data.table. Only character columns are converted: factorizing a numeric
# column (e.g. period/year) would break the `dt[period == year]` numeric
# comparisons in dt_filter_local().
factorize <- function(X) {
  dt <- as.data.table(X)
  char_cols <- names(dt)[vapply(dt, is.character, logical(1))]
  for (col in char_cols) {
    set(dt, j = col, value = as.factor(dt[[col]]))
  }
  dt
}

# (file name, output file stem) -> write the fast .rds
make_fast <- function(file, stem) {
  cat("Processing", file, "...\n")
  env <- new.env(parent = emptyenv())
  load(file.path(data_dir, file), envir = env)
  obj  <- get(ls(env)[1], envir = env)       # the single object in the .rda
  fast <- factorize(obj)
  path <- file.path(out_dir, paste0(stem, "_dt.rds"))
  saveRDS(fast, path, compress = "gzip")
  cat(sprintf(
    "  %-20s in-memory %.1f MB -> %.1f MB | on-disk %.1f MB | %s\n",
    stem,
    as.numeric(object.size(obj)) / 1e6,
    as.numeric(object.size(fast)) / 1e6,
    file.info(path)$size / 1e6,
    basename(path)
  ))
}

# Read each public dataset from data/, then build the fast copy
make_fast("data_gbd2021_cod.rda", "cod")
make_fast("data_gbd2021_sdg.rda", "sdg")
make_fast("data_gbd2021_lt.rda",  "lt")
