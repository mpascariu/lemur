# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed May 26 21:21:58 2021
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
      
      # The 3 main tabs defined in 3 separate modules
      tabPanel(
        title = icon("home"), 
        mod_home_ui("home")
      ), 
      
      tabPanel(
        title = icon("globe-africa"), 
        mod_map_ui("map_1")
      ),
      
      tabPanel(
        title = icon("info"), 
        mod_info_ui("info")
      )
    )
  )
}





