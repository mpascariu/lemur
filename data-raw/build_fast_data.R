# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Mon Aug 10 2026
# ------------------------------------------------- #

# Build fast-to-load copies of the package datasets.
#
# The package ships the GBD2021 tables as pre-factorized, gzip-compressed
# .rds files in inst/extdata/ that the Shiny app and the accessor functions
# (data_gbd2021_cod(), data_gbd2021_lt(), data_gbd2021_sdg()) load instead of
# lazy data objects:
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
#
# Source data: the process scripts (data-raw/process_gbd2021_*.R) write
# date-stamped .Rdata files into data-raw/IHME_GBD2021_Data/. Those are what
# this script reads. The .rda copies those scripts used to drop in data/ are
# gone -- keeping them out avoids shipping the same ~44 MB twice.

suppressPackageStartupMessages(library(data.table))

data_dir <- file.path("data-raw", "IHME_GBD2021_Data")
out_dir  <- file.path("inst", "extdata")
dir.create(out_dir, showWarnings = FALSE)

# Locate the most recent processed .Rdata for a dataset stem, e.g.
# data_gbd2021_cod_20260810.Rdata. The date-stamped names sort
# lexicographically in chronological order, so the last one is the newest.
latest_rdata <- function(stem) {
  pattern <- paste0("data_gbd2021_", stem, "_.*\\.Rdata$")
  files <- list.files(data_dir, pattern = pattern, full.names = TRUE)
  if (!length(files)) {
    stop("No processed data found for '", stem, "'. Run the corresponding\n",
         "data-raw/process_gbd2021_", stem, ".R script first (it writes a\n",
         ".Rdata file to data-raw/IHME_GBD2021_Data/).")
  }
  sort(files)[length(files)]
}

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

# (source .Rdata path, output file stem) -> write the fast .rds
make_fast <- function(file, stem) {
  cat("Processing", basename(file), "...\n")
  env <- new.env(parent = emptyenv())
  load(file, envir = env)
  obj  <- get(ls(env)[1], envir = env)       # the single object in the .Rdata
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

# Read the latest processed copy of each dataset, then build the fast .rds
make_fast(latest_rdata("cod"), "cod")
make_fast(latest_rdata("sdg"), "sdg")
make_fast(latest_rdata("lt"),  "lt")
