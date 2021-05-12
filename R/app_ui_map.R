# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed May 12 21:09:48 2021
# --------------------------------------------------- #

#' @keywords internal
mod_map_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      col_3(
        selectInput(
          ns("region"),
          "Region focus",
          choices = c("A", "B"),
          width = "100%"
        )
      ),
      col_3(
        shinyWidgets::radioGroupButtons(
          inputId = ns("source"),
          label = "Data source",
          choices = c(
            "GBD" = "GBD", 
            "UN" = "UN"), # "WHO",
          justified = TRUE,
          size = "sm"
        )
      ),
      col_3(
        shinyWidgets::radioGroupButtons(
          inputId = ns("indicator"),
          label = "Indicator",
          choices = c(
            "Cases" = "cases", 
            "Deaths" = "deaths"),
          justified = TRUE,
          size = "sm"
        )
      ),
      col_3(
        
      )
    ),
    
    fluidRow(
      col_2(
        uiOutput(ns("totals"))
      ),
      
      shinydashboard::box(
        width = 6, 
        solidHeader = TRUE,
        leaflet::leafletOutput(ns("map"))
      ),
      
      shinydashboard::box(
        width = 4, 
        solidHeader = TRUE,
        reactable::reactableOutput(ns("table"))
      )
    ),
    
    fluidRow(
      col_6(
        shinydashboard::box(
          width = NULL, 
          solidHeader = TRUE,
          title = tagList(
            tags$div(
              textOutput(ns("epicurve_title")), 
              style = "display: inline-block; font-weight: bold;") # ,
            # tags$div(tags$small("click + drag horizontally to zoom"), style = "display: inline-block;")
          ),
          highcharter::highchartOutput(ns("epicurve"))
        )
      ),
      
      col_6(
        shinydashboard::box(
          width = NULL, 
          solidHeader = TRUE,
          title = tagList(
            tags$div(
              textOutput(ns("cumulative_title")), 
              style = "display: inline-block; font-weight: bold;"
            ),
            tags$div(
              style = "display: inline-block; padding-right: 10px;"
            ),
            tags$div(
              style = "display: inline-block;",
              shinyWidgets::dropdownButton(
                size = "xs", 
                label = "params", 
                icon = icon("sliders"), # status = "primary",
                inline = TRUE, 
                width = "50px", 
                circle = FALSE,
                checkboxGroupInput(
                  ns("c_params"), 
                  label = "", 
                  inline = FALSE,
                  choices = c("log scale" = "log", 
                              "set days since" = "days")),
                checkboxInput(
                  ns("log"), 
                  label = "log scale", 
                  value = TRUE),
                checkboxInput(
                  ns("set_days"), 
                  label = "xaxis to days since...", 
                  value = FALSE),
                tags$br(),
                numericInput(
                  ns("n_days"), 
                  label = paste("n", "cases"), 
                  value = 10, 
                  min = 1, 
                  step = 10)
              )
            )
          ),
          highcharter::highchartOutput(ns("cumulative"))
        )
      )
    )
  )
}