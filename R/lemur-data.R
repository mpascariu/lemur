# ------------------------------------------------- #
# Author: Marius D. Pascariu
# ------------------------------------------------- #

# The three large combined GBD 2023 tables are stored in inst/extdata/ as
# pre-factorized, gzip-compressed data.tables (see data-raw/build_fast_data.R)
# and exposed through the accessor functions below. Keeping them out of data/
# avoids shipping the same ~44 MB twice -- once as lazy data and once as the
# .rds the app actually loads.

#' Abridged life table between 1990 and 2023 -- Global Burden of Disease Study 2023
#'
#' Life tables constructed using the GBD2023 probability of death data
#' (a single round covers 1990-2023). The terminal 95+ interval is left open;
#' ages run 0-95.
#'
#' Returns the combined GBD life tables used by the package, read from the
#' package's \file{inst/extdata/} directory as a pre-factorized
#' \code{data.table}.
#'
#' @source
#' Global Burden of Disease Collaborative Network.
#' Global Burden of Disease Study 2023 (GBD 2023) Results.
#' Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2024.
#' Available from https://vizhub.healthdata.org/gbd-results/.
#' \href{https://vizhub.healthdata.org/gbd-results/}{
#'  Global Burden of Disease Study 2023 (GBD 2023) Results}
#' @return A \code{data.table} of abridged life tables: one row per region,
#' sex, period and age group. Columns: \code{region}, \code{period},
#' \code{sex}, \code{x.int}, \code{x}, \code{mx}, \code{qx}, \code{ax},
#' \code{lx}, \code{dx}, \code{Lx}, \code{Tx} and \code{ex}.
#' @example inst/examples/data_gbd_lt.R


#' @export
data_gbd_lt <- function() {
  readRDS(system.file("extdata", "lt_dt.rds", package = "lemur"))
}


#' Causes of Death Data between 1990 and 2023 -- Global Burden of Disease Study 2023
#'
#' Combined cause-of-death counts from the Global Burden of Disease 2023 round,
#' covering 1990-2023. Read from the package's \file{inst/extdata/} directory as
#' a pre-factorized \code{data.table}.
#'
#' @source
#' Global Burden of Disease Collaborative Network.
#' Global Burden of Disease Study 2023 (GBD 2023) Results.
#' Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2024.
#' Available from https://vizhub.healthdata.org/gbd-results/.
#' \href{https://vizhub.healthdata.org/gbd-results/}{
#'  Global Burden of Disease Study 2023 (GBD 2023) Results}
#' @return A \code{data.table} of cause-of-death counts: one row per region,
#' sex, period, age group and cause. Columns: \code{x}, \code{region},
#' \code{sex}, \code{period}, \code{cause_name} and \code{deaths}.
#' @example inst/examples/data_gbd_cod.R


#' @export
data_gbd_cod <- function() {
  readRDS(system.file("extdata", "cod_dt.rds", package = "lemur"))
}

#' Causes of Death Data between 1990 and 2023 -- Global Burden of Disease Study 2023
#'
#' Combined cause-of-death counts from the Global Burden of Disease 2023 round,
#' grouped so that the evolution of the UN's Sustainable Development Goals can
#' be tracked. Read from the package's \file{inst/extdata/} directory as a
#' pre-factorized \code{data.table}.
#' @inherit data_gbd_cod source
#' @return A \code{data.table} of cause-of-death counts grouped for SDG
#' tracking: one row per region, sex, period, age group and cause. Columns:
#' \code{x}, \code{region}, \code{sex}, \code{period}, \code{cause_name} and
#' \code{deaths}.
#' @example inst/examples/data_gbd_sdg.R


#' @export
data_gbd_sdg <- function() {
  readRDS(system.file("extdata", "sdg_dt.rds", package = "lemur"))
}


#' Causes of Death List Mapped to ICD Codes
#'
#' This table contains the cause of death list used in the package
#' mapped to International Classification of Diseases (ICD) codes: ICD-10,
#' ICD-10 used in hospital/claim analyses and ICD-9.
#'
#' @examples
#' data_cod_mapping
"data_cod_mapping"

#' Input data for the shiny app
"data_app_input"

#' Simple features data for world country polygons used in mapping
#' @seealso \code{\link{plot_map}}
"data_sf"


# Hack CRAN check warnings related to tidyverse coding style
globalVariables(
  c("data_sf",
    "data_cod_mapping",
    "data_app_input"
  ))
