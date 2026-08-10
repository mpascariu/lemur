# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-08 18:18:18
# --------------------------------------------

# lemur Package

#' @details
#' To learn more about the package, start with the vignettes:
#' \code{browseVignettes(package = "lemur")}
#'
#' @import shinyWidgets
#' @import golem
#' @import ggplot2
#' @import data.table
#' @import sf
#' @import markdown
#'
#' @importFrom bslib page_navbar nav_panel nav_menu nav_item nav_spacer
#' @importFrom bslib bs_theme layout_columns layout_sidebar sidebar
#' @importFrom bslib card card_header card_body breakpoints
#' @importFrom pool dbPool poolClose

#' @importFrom shiny actionButton addResourcePath bookmarkButton br column
#' @importFrom shiny conditionalPanel div fluidPage fluidRow h3 icon
#' @importFrom shiny includeMarkdown navbarPage observe observeEvent reactive
#' @importFrom shiny reactiveValues selectInput shinyApp sliderInput shinyOptions
#' @importFrom shiny showNotification getShinyOption tabPanel tabsetPanel
#' @importFrom shiny updateSelectInput updateSliderInput validateCssUnit onStop
#' @importFrom shiny req bindCache
#'
#' @importFrom DBI dbConnect dbSendQuery dbFetch dbDisconnect
#' @importFrom RPostgres Postgres
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shinyjs useShinyjs
#' @importFrom MortalityLaws LifeTable
#' @importFrom glue glue_data
#' @importFrom purrr map
#'
#' @importFrom htmltools tags tagList tagAppendAttributes HTML
#' @importFrom htmltools findDependencies attachDependencies
#'
#' @importFrom tibble column_to_rownames rownames_to_column new_tibble as_tibble
#'
#' @importFrom tidyr pivot_wider pivot_longer replace_na
#'
#' @importFrom dplyr all_of arrange bind_rows bind_cols group_by left_join
#' @importFrom dplyr filter mutate mutate_all rename summarise select ungroup %>%
#'
#' @importFrom plotly ggplotly layout renderPlotly plotlyOutput
#'
#' @import leaflet
#' @importFrom leaflet.extras addFullscreenControl addResetMapButton
#'
#' @name MortalityCauses
#' @docType package
"_PACKAGE"
