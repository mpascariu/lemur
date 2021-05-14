# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Thu May 13 20:09:09 2021
# --------------------------------------------------- #

#' @keywords internal
mod_map_ui <- function(id) {
  ns <- NS(id)
  
  # shinyWidgets::setBackgroundColor(
  #   color = "ghost=white",
  #   gradient = c("linear", "radial"),
  #   direction = c("bottom", "top", "right", "left"),
  #   shinydashboard = FALSE
  # )
  
  tagList(
    col_2(
      tags$div(
        "Control Panel",
        style = "display: inline-block; font-weight: bold;"
      ),
    ),
    
    col_10(
      fluidRow(
        col_3(
          selectInput(
            ns("region"),
            "Region focus",
            choices = unique(data_gbd2019_lt$region),
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
              "Life Expectancy" = "a", 
              "Death rate" = "b"),
            justified = TRUE,
            size = "sm"
          )
        ),
        col_3(
          
        )
      ),
      
      
      fluidRow(
        shinydashboard::box(
          width = 7, 
          solidHeader = TRUE,
          leafletOutput(ns("map"))
        ),
        
        col_5(
        shinydashboard::box(
          width = NULL,
          solidHeader = TRUE,
          
          title = tagList(
            tags$div(
              "Cause of Death Distribution",
              style = "display: inline-block; font-weight: bold;"
            ),
            tags$div(
              style = "display: inline-block; padding-right: 1px;"
            ),
            tags$div(
              style = "display: inline-block;",
              shinyWidgets::dropdownButton(
                size = "xs",
                label = "params",
                icon = icon("sliders"),
                inline = TRUE,
                width = "50px",
                circle = FALSE,
                checkboxGroupInput(
                  ns("c_params"),
                  label = "",
                  inline = FALSE,
                  choices = c("log scale" = "log",
                              "percentage" = "perc"))

              )
            )
          )
        ),
        
        highcharter::highchartOutput(ns("table"))
        )
      ),
    
      fluidRow(
        col_6(
          shinydashboard::box(
            width = NULL, 
            solidHeader = TRUE,
            
            title = tagList(
              tags$div(
                "Cause Reduced Life Expectancy",
                style = "display: inline-block; font-weight: bold;"
              ),
              tags$div(
                style = "display: inline-block; padding-right: 1px;"
              ),
              tags$div(
                style = "display: inline-block;",
                shinyWidgets::dropdownButton(
                  size = "xs",
                  label = "params",
                  icon = icon("sliders"),
                  inline = TRUE,
                  width = "50px",
                  circle = FALSE,
                  checkboxGroupInput(
                    ns("c_params"),
                    label = "",
                    inline = FALSE,
                    choices = c("log scale" = "log",
                                "percentage" = "perc"))
                )
              )
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
                "Cause of Death / Age Decomposition", 
                style = "display: inline-block; font-weight: bold;"
              ),
              tags$div(
                style = "display: inline-block; padding-right: 1px;"
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
  )
}


