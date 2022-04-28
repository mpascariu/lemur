# --------------------------------------------------- #
# Author: Marius D. PASCARIU <mpascariu@scor.com>
# Last update: Wed Apr 27 15:19:32 2022
# --------------------------------------------------- #

#' Run the Shiny Application
#'
#' @param lb Launch browser. Default: TRUE. 
#' @param serverMode If TRUE the app will use data stored on the server, 
#' otherwise the datsets saved in the package. Default: FALSE.
#' @param ... A series of options to be used inside the app.
#' @examples 
#' \dontrun{
#' run_app()
#' }
#' @export
run_app <- function(..., lb = TRUE, serverMode = FALSE) {
  
  # requireNamespace("shinyBS", quietly=TRUE)
  library("shinyBS")
  
  shinyOptions(serverMode = serverMode)
  
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