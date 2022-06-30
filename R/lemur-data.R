# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Tue Nov 16 21:28:37 2021
# --------------------------------------------------- #
 

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
