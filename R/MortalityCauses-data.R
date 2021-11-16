# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Tue Nov 16 21:28:37 2021
# --------------------------------------------------- #

#' Abridged life table between 1990 and 2019 -- Global Burden of Disease Study 2019
#'
#' @source
#' United Nations, Department of Economic and Social Affairs,
#' Population Division (2019). World Population Prospects 2019,
#' \href{http://ghdx.healthdata.org/record/ihme-data/gbd-2019-life-tables-1950-2019}{
#'  Online Edition. Rev. 1.}
#'
#' @examples
#' data_gbd2019_lt
"data_gbd2019_lt"


#' Causes of Death Data between 1990 and 2019 -- Global Burden of Disease Study 2019
#'
#' @source
#' Global Burden of Disease Collaborative Network.
#' Global Burden of Disease Study 2019 (GBD 2019) Results.
#' Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020.
#' Available from http://ghdx.healthdata.org/gbd-results-tool.
#'
#' @examples
#' data_gbd2019_cod
"data_gbd2019_cod"

#' Causes of Death Data between 1990 and 2019 -- Global Burden of Disease Study 2019
#'
#' In this data object the death count data (GBD2019) is grouped in such a way
#' that make possible the tracking of the evolution of the UN's
#' Sustainable Development Goals.
#' @inherit data_gbd2019_cod source
#' @examples
#' data_gbd2019_sdg
"data_gbd2019_sdg"


#' Causes of Death List Mapped to ICD Codes
#'
#' This table contains the cause of death list used in the package
#' mapped to International Classification of Diseases (ICD) codes: ICD-10,
#' ICD-10 used in hospital/claim analyses, ICD-9 and ICD-9 used in
#' hospital/claim analyses.
#'
#' @examples
#' data_cod_mapping
"data_cod_mapping"

#' Input data for the shiny app
"data_app_input"

#' Simple features data for the map
#' @seealso \code{\link{plot_map}}
"data_sf"


# Hack CRAN check warnings related to tidyverse coding style
globalVariables(
  c("data_sf",
    "data_gbd2019_lt",
    "data_gbd2019_cod",
    "data_gbd2019_sdg",
    "data_cod_mapping",
    "data_app_input"
  ))
