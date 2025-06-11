# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Fri Jun  6 08:59:54 2025
# ------------------------------------------------- #



#' UI - data page
#' @keywords internal
#' @export
ui_datatab <- function() {
  layout_columns(
    col_widths = 12,
    data_panel()
  )
}


#' @keywords internal
data_panel <- function() {
  tabsetPanel(
    type = "tabs",
    # Life Table Data Tab
    tabPanel(
      title = "Life Table Data",
      tagList(
        layout_columns(
          col_widths = 12,  # Full width
          style = "margin-bottom: 2rem;", 
          div( 
            style = "width: 100%; padding: 0 18px;",
            DT::dataTableOutput("lt_initial")
          )
        ),
        layout_columns(
          col_widths = 12,
          style = "margin-bottom: 2rem;",
          div(
            style = "width: 100%; padding: 0 18px;",
            DT::dataTableOutput("lt_final")
          )
        )
      )
    ),
    # Cause of Death Data Tab
    tabPanel(
      title = "Cause of Death Data",
      tagList(
        layout_columns(
          col_widths = 12,
          style = "margin-bottom: 2rem;",
          div(
            style = "width: 100%;",
            DT::dataTableOutput("cod_initial")
          )
        ),
        layout_columns(
          col_widths = 12,
          div(
            style = "width: 100%;",
            DT::dataTableOutput("cod_final")
          )
        )
      )
    ),
    # Decomposition Data Tab
    tabPanel(
      title = "Decomposition Data",
      layout_columns(
        col_widths = 12,
        div(
          style = "width: 100%;",
          DT::dataTableOutput("decomposition_data")
        )
      )
    ),
    # Reduction Matrix Tab
    tabPanel(
      title = "Reduction Matrix",
      layout_columns(
        col_widths = 12,
        div(
          style = "width: 100%;",
          DT::dataTableOutput("reduction_matrix")
        )
      )
    )
  )
}









