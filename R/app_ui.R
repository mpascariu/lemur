# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Tue Jun 01 21:52:31 2021
# --------------------------------------------------- #

#' The application User-Interface
#' 
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
      
      # The 6 main tabs defined in 6 separate modules
      tabPanel(
        title = icon("home"), 
        # mod_home_ui("home")
        ui_home()
      ), 
      
      tabPanel( # Dashboard
        title = icon("globe-africa"), 
        ui_dashbord()
      ),
      
      tab_md( # "Methods Protocol"
        title = icon("calculator"),
        file = 'app/www/doc_methods.md'
      ),
      
      tab_md( # sources
        title = icon("database"),
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


#' UI - home page
#' @keywords internal
ui_home <- function() {
  
  div(
    style = '
    height: 897px; 
    width: 2100px; 
    background: url(www/background_mountain.jpg) no-repeat center center fixed;
    ',
    
    HTML(r'(

      <h1>This is a heading</h1>
      <p class="my-class">This is some text!</p>
      <ul>
        <li>First bullet</li>
        <li>Second bullet</li>
      </ul>
    )')
  )
  
}


#' @keywords internal
tab_md <- function(title, file) {
  
  tabPanel(
    title = title,
    column(
      width = 10,
      offset = 1,
      includeMarkdown(
        system.file(file, package = 'MortalityCauses')
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

