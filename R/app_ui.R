# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Wed Jun 11 11:38:08 2025
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
      layout_columns(
        col_widths = 10,
        col_offsets = 1,
        includeMarkdown(system.file('app/www/doc_methods.md', package = 'lemur'))
      )
    ),
    nav_panel(
      title = "Sources", 
      icon = icon("book"),
      layout_columns(
        col_widths = 10,
        col_offsets = 1,
        includeMarkdown(system.file('app/www/doc_sources.md', package = 'lemur'))
      )
    ),
    nav_panel(
      title = "About", 
      icon = icon("info-circle"),
      layout_columns(
        col_widths = 10,
        col_offsets = 1,
        includeMarkdown(system.file('app/www/doc_about.md', package = 'lemur'))
      )
    ),
    nav_panel(
      title = "Contact", 
      icon = icon("address-book"),
      layout_columns(
        col_widths = 10,
        col_offsets = 1,
        includeMarkdown(system.file('app/www/doc_contact.md', package = 'lemur'))
      )
    )
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
golem_add_external_resources <- function() {
  addResourcePath(
    'www', system.file('app/www', package = 'lemur')
  )
  
  tags$head(
    golem::activate_js(),
    shinyjs::useShinyjs(),
    tags$link(
      href = "https://fonts.googleapis.com/css?family=Roboto+Condensed:400,700&display=swap",
      rel = "stylesheet"
    ),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "www/styles.css"
    ),
    tags$script(src = "www/addNavLink.js")
  )
}


