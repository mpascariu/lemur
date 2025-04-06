# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Sun Apr  6 22:15:37 2025
# ------------------------------------------------- #


#' Abridged life table between 1990 and 2021 -- Global Burden of Disease Study 2022
#' Life tables constructed using the GBD2021 probability of death data.
#' 
#' @source
#' Global Burden of Disease Collaborative Network.
#' Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2022.
#' Available from https://vizhub.healthdata.org/gbd-results/.
#' \href{https://vizhub.healthdata.org/gbd-results/}{
#'  Global Burden of Disease Study 2021 (GBD 2021) Results}
#' @examples
#' data_gbd2021_lt
"data_gbd2021_lt"


#' Causes of Death Data between 1990 and 2021 -- Global Burden of Disease Study 2022
#'
#' @source
#' Global Burden of Disease Collaborative Network.
#' Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2022.
#' Available from https://vizhub.healthdata.org/gbd-results/.
#' \href{https://vizhub.healthdata.org/gbd-results/}{
#'  Global Burden of Disease Study 2021 (GBD 2021) Results}
#' @examples
#' data_gbd2021_cod
"data_gbd2021_cod"

#' Causes of Death Data between 1990 and 2021 -- Global Burden of Disease Study 2022
#'
#' In this data object the death count data (GBD2021) is grouped in such a way
#' that make possible the tracking of the evolution of the UN's
#' Sustainable Development Goals.
#' @inherit data_gbd2021_cod source
#' @examples
#' data_gbd2021_sdg
"data_gbd2021_sdg"


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
    "data_gbd2021_lt",
    "data_gbd2021_cod",
    "data_gbd2021_sdg",
    "data_cod_mapping",
    "data_app_input"
  ))
