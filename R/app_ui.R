# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed May 12 20:43:16 2021
# --------------------------------------------------- #

#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#' @noRd
app_ui <- function() {
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    navbarPage(
      title = tagList("Mortality Causes"),
      windowTitle = "Mortality Causes",
      position = "fixed-top",
      collapsible = TRUE,
      
      # tabs 
      tabPanel(
        title = icon("globe-africa"), 
        mod_map_ui("map_1")
      ), #"Global overview", 
      
      tabPanel(
        #title = "About",
        title = icon("info"), 
        fluidRow(
          column(
            width = 8, 
            offset = 2,
            tabsetPanel(
              tabPanel(
                "About", 
                includeMarkdown(
                  system.file('app/www/about.md', package = 'MortalityCauses')
                )
              ),
              tabPanel(
                "Methods Protocol", 
                includeMarkdown(
                  system.file('app/www/trends.md', package = 'MortalityCauses')
                )
              )
            )
          )
        )
      )
      
    ),
    # waiter::waiter_show_on_load(html = waiter::spin_3()),
    # waiter::waiter_hide_on_render("map_1-cumulative")
  )
}



#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'MortalityCauses')
  )
  
  tags$head(
    metathis::meta() %>%
      metathis::meta_social(
        title = "MortalityCauses Dashboard",
        description = "Developed by Pascariu et al.",
        url = "https://github.com/mpascariu",  # to be updated
        # image = "",
        image_alt = "MortalityCauses",
        twitter_card_type = "summary_large_image"
      ),
    
    golem::activate_js(),
    tags$link(
      href = "https://fonts.googleapis.com/css?family=Roboto+Condensed:400,700&display=swap", 
      rel = "stylesheet"),
    # waiter::use_waiter(),
    shinyjs::useShinyjs(),
    tags$link(
      rel="stylesheet", 
      type="text/css", 
      href="www/styles.css"),
    tags$script(src="www/addNavLink.js"),
    shinyWidgets::useShinydashboard()
  )
}

