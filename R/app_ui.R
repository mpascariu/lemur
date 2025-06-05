# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Thu Jun  5 20:02:16 2025
# ------------------------------------------------- #

#' The application User-Interface
#'
#' @keywords internal
#' @export
app_ui <- function() {
  theme = bslib::bs_theme(version = 5)
  
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
  page_navbar(
    title = "Life Expectancy Monitor",
    theme = bs_theme(version = 5, bootswatch = "bootstrap"),
    nav_panel(
      title = "Dashboard", 
      icon = icon("globe-africa"),
      ui_dashbord()
    ),
    nav_panel(
      title = "Data", 
      icon = icon("database"),
      ui_datatab()
    ),
    
    nav_panel(
      title = "Methods", 
      icon = icon("calculator"),
      column(
        width = 10, offset = 1,
        includeMarkdown(system.file('app/www/doc_methods.md', package = 'lemur'))
      )
    ),
    nav_panel(
      title = "Sources", 
      icon = icon("book"),
      column(
        width = 10, offset = 1,
        includeMarkdown(system.file('app/www/doc_sources.md', package = 'lemur'))
      )
    ),
    nav_panel(
      title = "About", 
      icon = icon("info-circle"),
      column(
        width = 10, offset = 1,
        includeMarkdown(system.file('app/www/doc_about.md', package = 'lemur'))
      )
    ),
    nav_panel(
      title = "Contact", 
      icon = icon("address-book"),
      column(
        width = 10, offset = 1,
        includeMarkdown(system.file('app/www/doc_contact.md', package = 'lemur'))
      )
    )
    
    # Additional nav_panel() entries for other tabs
  )
}


#' UI - markdown pages
#' @keywords internal
#' @export
tab_md <- function(title, file) {
  nav_panel(
    title = title,
    layout_columns(
      col_widths = c(1, 10, 1),
      gap = "0px",
      fillable = TRUE,
      !!!list(
        NULL,
        includeMarkdown(
          system.file(file, package = 'lemur')
        ),
        NULL
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

