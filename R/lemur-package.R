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
#' @importFrom shiny req bindCache tags tagList HTML
#'
#' @importFrom RPostgres Postgres
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shinyjs useShinyjs
#'
#' @importFrom dplyr all_of arrange bind_rows bind_cols group_by left_join
#' @importFrom dplyr filter mutate mutate_all rename summarise select ungroup %>%
#'
#' @importFrom plotly renderPlotly plotlyOutput
#'
#' @import leaflet
#' @importFrom leaflet.extras addFullscreenControl addResetMapButton
#'
#' @name MortalityCauses
#' @docType package
"_PACKAGE"
