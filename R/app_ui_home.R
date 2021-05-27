# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Thu May 27 09:26:22 2021
# --------------------------------------------------- #

#' @keywords internal
mod_home_ui <- function(id) {
  ns <- NS(id)

  # tags$style(
  #   HTML("
  #       {
  #        background-color: #000;
  #        color: #FFF;
  #       }"
  #   )
  # )
  
  
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


  # shinyWidgets::setBackgroundImage(
  #    src = "www/background_homepage.jpg",
  #    shinydashboard = FALSE
  #   )

}