# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed May 12 20:58:10 2021
# --------------------------------------------------- #

#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  callModule(mod_map_server, "map_1")
  
}
