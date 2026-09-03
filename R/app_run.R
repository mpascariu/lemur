# ------------------------------------------------- #
# Author: Marius D. Pascariu
# ------------------------------------------------- #


#' Run the Shiny Application
#'
#' @param lb Launch browser. Default: TRUE.
#' @param serverMode If TRUE, the app uses data stored on the server;
#' otherwise, the datasets saved in the package. Default: FALSE.
#' @param ... A series of options to be used inside the app.
#' @example inst/examples/run_app.R



#' @export
run_app <- function(..., lb = TRUE, serverMode = FALSE) {
  shinyOptions(serverMode = serverMode)
  
  # `...` is accepted for backward compatibility with the old golem-based
  # launch (golem_opts), but the app no longer consumes any such options.
  shinyApp(
    ui = app_ui,
    server = app_server,
    options = list(launch.browser = lb),
    enableBookmarking = "server",
    uiPattern = "/"
  )
}
