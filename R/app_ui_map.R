# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Thu May 27 08:49:32 2021
# --------------------------------------------------- #

#' @keywords internal
mod_map_ui <- function(id) {
  ns <- NS(id)
  
  tagList(

    column(
      width = 2,

      shinyWidgets::radioGroupButtons(
        inputId = ns("sex"),
        label   = "Sex",
        choices = c(
          "Female" = "female", 
          "Male"   = "male",
          "Both"   = "both"),
        selected = "both",
        justified = TRUE,
        size = "sm"
      ),
      
      selectInput(
        inputId  = ns("region1"),
        label    = "Region",
        choices  = unique(data_gbd2019_lt$region),
        selected = "Romania",
        width    = "100%"
      ), 
      
      conditionalPanel(
        condition = "input.mode == cntr_compare",
        selectInput(
          inputId  = ns("region2"),
          label    = "Region 2",
          choices  = unique(data_gbd2019_lt$region),
          selected = "Mexico",
          width    = "100%"
        ), 
      ),
      
      sliderInput(
        inputId = "cod_change",
        label = "Modify the case-specific risk of dying:",
        post = "%",
        value = 0,
        min = -99,
        max = 100,
        step = 1),
      
      prettyCheckboxGroup(
        inputId = "cod_target",
        label = "Which cause of death to be affected:", 
        choices = unique(data_gbd2019_cod$cause_name),
        icon = icon("check"),
        status = "success",
        animation = "rotate",
        outline = TRUE,
        inline = FALSE
      )
    ),
    
    column(
      width = 10,
      fluidRow(
        column(
          width = 3,
          shinyWidgets::radioGroupButtons(
            inputId = ns("mode"),
            label   = "Mode",
            choices = c(
              "COD Risk Change" = "cod_change", 
              "Country Comparisons" = "cntr_compare"),
            justified = TRUE,
            size = "sm"
          )
        ),

        column(
          width = 2,
          shinyWidgets::radioGroupButtons(
            inputId = ns("legend_labs"),
            label   = "Legend Labels",
            choices = c(
              "Long"  = "long", 
              "Short" = "short"),
            justified = TRUE,
            size = "sm"
          )
        )
      ),
      
      
      fluidRow(
        style = 'padding-top:0px; padding-bottom:0px;',
        
        column(
          width = 7,
          style ='padding-right:0px; padding-top:0px; padding-bottom:0px',
          
          shinydashboard::box(
            width = NULL,
            style = 'padding:0px',
            solidHeader = TRUE,
            
            title = tagList(
              tags$div(
                "World Map",
                style = "display: inline-block; font-weight: bold;"
              )
            ),
            
            leafletOutput(
              outputId = ns("figure1"),
              height = 381
            )
          )
        ),
        
        column(
          width = 5,
          style='padding:0px;',
          
          shinydashboard::box(
            width = NULL,
            solidHeader = TRUE,
            
            title = tagList(
              tags$div(
                "Difference in Life Expectancy",
                style = "display: inline-block; font-weight: bold; padding:0px;"
              ),
              tags$div(
                style = "display: inline-block; padding-left: 340px;",
                shinyWidgets::dropdownButton(
                  size = "xs",
                  label = "params",
                  right = TRUE,
                  icon = icon("sliders"),
                  inline = TRUE,
                  width = "10px",
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
            
            plotOutput(
              outputId = ns("figure2"),
              height = 360
            )
          )
        )
      ),
    
      fluidRow(
        style = 'padding-top:0px; padding-bottom:0px; margin-top:-50%;',
        
        column(
          width = 6,
          style = 'padding-right:0px; padding-top:0px; padding-bottom:0px',
          
          shinydashboard::box(
            width = NULL, 
            solidHeader = TRUE,
            
            title = tagList(
              tags$div(
                "Cause of Death Distribution",
                style = "display: inline-block; font-weight: bold; padding:0px;"
              ),
              tags$div(
                style = "display: inline-block; padding-left: 400px;",
                shinyWidgets::dropdownButton(
                  size = "xs",
                  label = "params",
                  right = TRUE,
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
            
            plotOutput(
              outputId = ns("figure3"),
              height = 330
            )
          )
        ),
  
        column(
          width = 6,
          style='padding:0px;',
          
          shinydashboard::box(
            width = NULL, 
            solidHeader = TRUE,
            
            title = tagList(
              tags$div(
                "Cause of Death / Age Decomposition", 
                style = "display: inline-block; font-weight: bold; padding:0px;"
              ),
              tags$div(
                style = "display: inline-block; padding-left: 400px;",
                shinyWidgets::dropdownButton(
                  size = "xs", 
                  label = "params",
                  right = TRUE,
                  icon = icon("sliders"),
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
            
            plotOutput(
              outputId = ns("figure4"),
              height = 330
            )
          )
        )
      )
    )
  )
}


