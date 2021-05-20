# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed May 19 11:47:33 2021
# --------------------------------------------------- #

# MortalityCauses Package

#' @details
#' To learn more about the package, start with the vignettes:
#' \code{browseVignettes(package = "MortalityCauses")}
#'
#' @import leaflet
#' @importFrom leaflet.extras addFullscreenControl addResetMapButton 
#' @importFrom dplyr arrange all_of bind_rows bind_cols 
#' group_by mutate mutate_all select ungroup
#' @importFrom tidyr pivot_wider pivot_longer replace_na
#' @importFrom tibble column_to_rownames rownames_to_column new_tibble
#' @importFrom forcats as_factor
#' @importFrom MortalityLaws LifeTable
#' @importFrom magrittr %>%
#' @importFrom utils head tail
#' 
#' @import shiny
#' @import ggplot2
#' @import golem
#' @importFrom config get
#' @importFrom metathis meta meta_social
#' @importFrom shinyWidgets useShinydashboard setBackgroundImage
#' @importFrom shinyjs useShinyjs
#' @importFrom shinydashboard box
#' @importFrom reactable reactableOutput
#' @importFrom htmltools tags tagList tagAppendAttributes HTML
#' @importFrom highcharter highchartOutput
#' @importFrom rmarkdown render
#' @importFrom markdown markdownToHTML
#' @name MortalityCauses
#' @docType package
"_PACKAGE"
 
