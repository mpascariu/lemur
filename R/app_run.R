# --------------------------------------------------- #
# Author: Marius D. PASCARIU <mpascariu@scor.com>
# Last update: Wed Apr 27 15:19:32 2022
# --------------------------------------------------- #

#' Run the Shiny Application
#'
#' @param lb Launch browser. Default: TRUE. 
#' @param ... A series of options to be used inside the app.
#' @export
run_app <- function(..., lb = TRUE) {
  
  # requireNamespace("shinyBS", quietly=TRUE)
  library("shinyBS")
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server, 
      options = list("launch.browser" = lb),
      enableBookmarking = "server",
      uiPattern = "/"
      ), 
    golem_opts = list(...)
  )
}