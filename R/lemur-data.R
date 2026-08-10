# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Sun Aug 10 2026
# ------------------------------------------------- #

# The three large GBD2021 tables are stored in inst/extdata/ as pre-factorized,
# gzip-compressed data.tables (see data-raw/build_fast_data.R) and exposed
# through the accessor functions below. Keeping them out of data/ avoids
# shipping the same ~44 MB twice -- once as lazy data and once as the .rds the
# app actually loads.

#' Abridged life table between 1990 and 2021 -- Global Burden of Disease Study 2022
#'
#' Life tables constructed using the GBD2021 probability of death data.
#'
#' Returns the GBD2021 life tables used by the package, read from the package's
#' \file{inst/extdata/} directory as a pre-factorized \code{data.table}.
#'
#' @source
#' Global Burden of Disease Collaborative Network.
#' Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2022.
#' Available from https://vizhub.healthdata.org/gbd-results/.
#' \href{https://vizhub.healthdata.org/gbd-results/}{
#'  Global Burden of Disease Study 2021 (GBD 2021) Results}
#' @return A \code{data.table} of abridged life tables: one row per region,
#' sex, period and age group. Columns: \code{region}, \code{period},
#' \code{sex}, \code{x.int}, \code{x}, \code{mx}, \code{qx}, \code{ax},
#' \code{lx}, \code{dx}, \code{Lx}, \code{Tx} and \code{ex}.
#' @examples
#' L <- data_gbd2021_lt()
#' L[L$region == "Romania" & L$sex == "both" & L$period == 2021, ]
#' @export
data_gbd2021_lt <- function() {
  readRDS(system.file("extdata", "lt_dt.rds", package = "lemur"))
}


#' Causes of Death Data between 1990 and 2021 -- Global Burden of Disease Study 2022
#'
#' Returns the GBD2021 cause-of-death counts used by the package, read from the
#' package's \file{inst/extdata/} directory as a pre-factorized
#' \code{data.table}.
#'
#' @source
#' Global Burden of Disease Collaborative Network.
#' Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2022.
#' Available from https://vizhub.healthdata.org/gbd-results/.
#' \href{https://vizhub.healthdata.org/gbd-results/}{
#'  Global Burden of Disease Study 2021 (GBD 2021) Results}
#' @return A \code{data.table} of cause-of-death counts: one row per region,
#' sex, period, age group and cause. Columns: \code{x}, \code{region},
#' \code{sex}, \code{period}, \code{cause_name} and \code{deaths}.
#' @examples
#' D <- data_gbd2021_cod()
#' D[D$region == "Romania" & D$sex == "both" & D$period == 2021, ]
#' @export
data_gbd2021_cod <- function() {
  readRDS(system.file("extdata", "cod_dt.rds", package = "lemur"))
}

#' Causes of Death Data between 1990 and 2021 -- Global Burden of Disease Study 2022
#'
#' Returns the GBD2021 cause-of-death counts grouped so that the evolution of
#' the UN's Sustainable Development Goals can be tracked. Read from the
#' package's \file{inst/extdata/} directory as a pre-factorized
#' \code{data.table}.
#' @inherit data_gbd2021_cod source
#' @return A \code{data.table} of cause-of-death counts grouped for SDG
#' tracking: one row per region, sex, period, age group and cause. Columns:
#' \code{x}, \code{region}, \code{sex}, \code{period}, \code{cause_name} and
#' \code{deaths}.
#' @examples
#' S <- data_gbd2021_sdg()
#' str(S)
#' @export
data_gbd2021_sdg <- function() {
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
