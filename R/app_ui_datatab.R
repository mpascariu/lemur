# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Tue Mar 15 15:11:11 2022
# --------------------------------------------------- #

#' UI - data page
#' @keywords internal
#' @export
ui_datatab <- function() {
  tagList(
    column(
      width = 12,
      offset = 0,
      data_panel()
    )
  )
}

#' @keywords internal
data_panel <- function() {
  
  tabsetPanel(
    type = "tabs",
    tabPanel(
      title = "Life Table Data",
      fluidPage(
        
        fluidRow(
          column(
            width = 10,
            offset = 1,
            DT::dataTableOutput("lt_initial"),
          ),
        ),
        
        fluidRow(
          column(
            width = 10,
            offset = 1,
            DT::dataTableOutput("lt_final")
          )
        )
      )
    ),
    
    tabPanel(
      title = "Cause of Death Data", 
      fluidPage(
        
        fluidRow(
          column(
            width = 12,
            offset = 0,
            DT::dataTableOutput("cod_initial")
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            offset = 0,
            DT::dataTableOutput("cod_final")
          )
        ),
      )
    ),
    
    tabPanel(
      title = "Decomposition Data", 
      fluidRow(
        column(
          width = 12,
          offset = 0,
          DT::dataTableOutput("decomposition_data")
        )
      ),
    )
  )
  

}





















