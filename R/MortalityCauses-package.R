# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Thu Nov 11 20:16:32 2021
# --------------------------------------------------- #

# MortalityCauses Package

#' @details
#' To learn more about the package, start with the vignettes:
#' \code{browseVignettes(package = "MortalityCauses")}
#'
#' @import shiny
#' @import shinyBS
#' @import shinyWidgets
#' @import golem
#' @import ggplot2
#' @import pals
#' @import data.table
#' @import sf
#'
#' @importFrom shinydashboard box
#' @importFrom shinyjs useShinyjs
#' @importFrom scales label_number_si
#' @importFrom forcats as_factor
#' @importFrom MortalityLaws LifeTable
#' @importFrom glue glue_data
#' @importFrom purrr map
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
#' @importFrom tibble
#' column_to_rownames
#' rownames_to_column
#' new_tibble
#' as_tibble
#'
#' @importFrom tidyr
#' pivot_wider
#' pivot_longer
#' replace_na
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
#' rename
#' summarise
#' select
#' ungroup
#' %>%
#'
#' @importFrom plotly
#' ggplotly
#' layout
#' renderPlotly
#' plotlyOutput
#'
#' @import leaflet
#' @importFrom leaflet.extras
#' addFullscreenControl
#' addResetMapButton
#'
#' @name MortalityCauses
#' @docType package
"_PACKAGE"

