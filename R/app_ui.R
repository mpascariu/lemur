# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Sun May  4 16:33:57 2025
# ------------------------------------------------- #

#' The application User-Interface
#'
#' @keywords internal
#' @export
app_ui <- function() {

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    ui_tabs()
  )
}


#' UI -  List the first level UI elements here
#' @keywords internal
#' @export
ui_tabs <- function() {
  tagList(
    navbarPage(
      title       = tagList("Life Expectancy Monitor"),
      windowTitle = "Life Expectancy Monitor",
      position    = "fixed-top",
      collapsible = TRUE,

      # The 5 main tabs defined in 5 separate modules

      tabPanel( # Dashboard
        title = icon("globe-africa"),
        ui_dashbord()
      ),
      
      tabPanel( # Dashboard
        title = icon("database"),
        ui_datatab()
      ),

      tab_md( # "Methods Protocol"
        title = icon("calculator"),
        file = 'app/www/doc_methods.md'
      ),

      tab_md( # sources
        title = icon("book"),
        file = 'app/www/doc_sources.md'
      ),

      tab_md( # About
        title = icon("info-circle"),
        file = 'app/www/doc_about.md'
      ),

      tab_md( # Contact
        title = icon("address-book"),
        file = 'app/www/doc_contact.md'
      )
    )
  )
}

#' UI - markdown pages
#' @keywords internal
#' @export
tab_md <- function(title, file) {

  tabPanel(
    title = title,
    column(
      width = 10,
      offset = 1,
      includeMarkdown(
        system.file(file, package = 'lemur')
      )
    )
  )
}



#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @keywords internal
#' @export
golem_add_external_resources <- function(){

  addResourcePath(
    'www', system.file('app/www', package = 'lemur')
  )

  tags$head(
    # metathis::meta() %>%
    #   metathis::meta_social(
    #     title = "lemur Dashboard",
    #     description = "Developed by Pascariu et al.",
    #     url = "https://github.com/mpascariu",  # to be updated
    #     # image = "",
    #     image_alt = "lemur",
    #     twitter_card_type = "summary_large_image"
    #   ),

    golem::activate_js(),
    tags$link(
      href = "https://fonts.googleapis.com/css?family=Roboto+Condensed:400,700&display=swap",
      rel = "stylesheet"),
    shinyjs::useShinyjs(),
    tags$link(
      rel="stylesheet",
      type="text/css",
      href="www/styles.css"),
    tags$script(src="www/addNavLink.js"),
    useShinydashboard_()
  )
}

