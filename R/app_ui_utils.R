# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Thu May 27 08:28:03 2021
# --------------------------------------------------- #

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    text-align: left;
    padding-left: 2px; 
    padding-right: 2px; 
    color: rgba(85, 85, 85);
    font-size: 12px;
    font-family: Arial;
    background: rgba(255,255,255,0.8);
    box-shadow: 0 0 15px rgba(0,0,0,0.2);
    border-radius: 5px;
  }
"))


#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @keywords internal
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