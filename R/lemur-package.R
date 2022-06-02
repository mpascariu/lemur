# --------------------------------------------------- #
# Author: Marius D. PASCARIU <mpascariu@scor.com>
# Last update: Tue Apr 26 16:26:21 2022
# --------------------------------------------------- #

# lemur Package

#' @details
#' To learn more about the package, start with the vignettes:
#' \code{browseVignettes(package = "lemur")}
#'
#' @import shinyBS
#' @import shinyWidgets
#' @import golem
#' @import ggplot2
#' @import data.table
#' @import sf
#' @import shinydashboard
#'
#' @importFrom shiny 
#' actionButton 
#' addResourcePath 
#' bookmarkButton 
#' br
#' column
#' conditionalPanel 
#' div 
#' fluidPage 
#' fluidRow 
#' h3 
#' icon
#' includeMarkdown 
#' navbarPage 
#' observeEvent 
#' reactive
#' selectInput 
#' shinyApp 
#' sliderInput 
#' shinyOptions
#' showNotification
#' getShinyOption
#' tabPanel 
#' tabsetPanel
#' updateSelectInput 
#' updateSliderInput 
#' validateCssUnit
#' 
#' @importFrom DBI dbConnect dbSendQuery dbFetch dbDisconnect
#' @importFrom RPostgres Postgres
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shinyjs useShinyjs
#' @importFrom MortalityLaws LifeTable
#' @importFrom glue glue_data
#' @importFrom purrr map
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

