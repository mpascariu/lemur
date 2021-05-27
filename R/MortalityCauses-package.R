# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed May 26 21:45:28 2021
# --------------------------------------------------- #

# MortalityCauses Package

#' @details
#' To learn more about the package, start with the vignettes:
#' \code{browseVignettes(package = "MortalityCauses")}
#'
#' @import shiny
#' @import shinyWidgets
#' @import ggplot2
#' @import golem
#' @import leaflet
#' 
#' @importFrom reactable reactableOutput
#' @importFrom shinyjs useShinyjs
#' @importFrom shinydashboard box
#' @importFrom scales label_number_si
#' @importFrom config get
#' @importFrom rmarkdown render
#' @importFrom markdown markdownToHTML
#' @importFrom forcats as_factor
#' @importFrom MortalityLaws LifeTable
#' @importFrom magrittr %>%
#' 
#' @importFrom leaflet.extras 
#' addFullscreenControl 
#' addResetMapButton 
#' 
#' @importFrom dplyr 
#' all_of 
#' arrange 
#' bind_rows 
#' bind_cols 
#' group_by 
#' left_join 
#' filter
#' mutate 
#' mutate_all 
#' summarise
#' select 
#' ungroup 
#' 
#' @importFrom tidyr 
#' pivot_wider 
#' pivot_longer 
#' replace_na
#' 
#' @importFrom tibble 
#' column_to_rownames 
#' rownames_to_column 
#' new_tibble
#' 
#' @importFrom metathis 
#' meta 
#' meta_social
#' 
#' @importFrom htmltools 
#' tags 
#' tagList 
#' tagAppendAttributes 
#' HTML
#' 
#' @name MortalityCauses
#' @docType package
"_PACKAGE"
 
