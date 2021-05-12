# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed May 12 20:57:43 2021
# --------------------------------------------------- #

#' Run the Shiny Application
#'
#' @param lb Launch browser. Default: TRUE. 
#' @param ... A series of options to be used inside the app.
#' @export
run_app <- function(..., lb = TRUE) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server, 
      options = list("launch.browser" = lb)), 
    golem_opts = list(...)
  )
}