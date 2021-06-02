# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed Jun 02 10:41:26 2021
# --------------------------------------------------- #

#' @keywords internal
top_panel <- function() {
  fluidRow(
    column(
      width = 3,
      shinyWidgets::radioGroupButtons(
        inputId = "mode",
        label   = "Dashboard Mode",
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
        inputId = "legend_labs",
        label   = "Legend Labels",
        choices = c(
          "Long"  = "long", 
          "Short" = "short"),
        justified = TRUE,
        size = "sm"
      )
    ),
    
    column(
      width = 1,
      offset = 6,
      
      tags$div(
        style = "padding-top: 25px;"
      ), 
      
      switchInput(
        inputId = "perc",
        onStatus = "success", 
        offStatus = "danger",
        label = icon("percent")
      )
    )
  )
}


#' @keywords internal
side_panel <- function() {
  tagList(
    
    shinyWidgets::radioGroupButtons(
      inputId = "sex",
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
      inputId  = "region1",
      label    = "Region",
      choices  = unique(data_gbd2019_lt$region),
      selected = "Romania",
      width    = "100%"
    ), 
    
    conditionalPanel(
      condition = "input.mode == cntr_compare",
      selectInput(
        inputId  = "region2",
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
      min = -100,
      max = 100,
      step = 5),
    
    sliderTextInput(
      inputId = "age_change",
      label = "Which age interval to modify?",
      choices = unique(data_gbd2019_lt$x),
      selected = c(0, 110),
      grid = TRUE
    ),
    
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
  )
}


#' @keywords internal
main_panel <- function() {
  tagList(
    fluidRow(
      column(
        width = 7,
        style ='padding-right:0px; padding-top:0px; padding-bottom:0px',
        
        boxFrame(
          style = 'padding:0px',
          title = tagList(
            tags$div(
              "World Map",
              style = "display: inline-block; font-weight: bold; padding:0px;"
            )
          ),
          
          leafletOutput(
            outputId = "figure1",
            height = 381
          )
        )
      ),
      
      column(
        width = 5,
        style='padding:0px;',
        
        boxFrame(
          title = boxTitleInput(
            title = "Difference in Life Expectancy",
            db_style = "padding-left: 340px;"
          ),
          
          plotOutput(
            outputId = "figure2",
            height = 360
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        style = 'padding-right:0px; padding-top:0px; padding-bottom:0px',
        
        boxFrame(
          title = boxTitleInput(
            title = "Cause of Death Distribution",
            db_style = "padding-left: 450px;"
          ),
          
          plotOutput(
            outputId = "figure3",
            height = 330
          )
        )
      ),
      
      column(
        width = 6,
        style='padding:0px;',
        
        boxFrame(
          title = boxTitleInput(
            title = "Cause of Death / Age Decomposition",
            db_style = "padding-left: 410px;"
          ),
          
          plotOutput(
            outputId = "figure4",
            height = 330
          )
        )
      )
    )
  )
}


#' @keywords internal
boxFrame <- function(..., 
                     width = NULL, 
                     solidHeader = TRUE, 
                     style = NULL) {
  shinydashboard::box(
    width = width, 
    solidHeader = solidHeader,
    style = style,
    ...
  )
}


#' @keywords internal
boxTitleInput <- function(title, db_style, ...) {
  
  tagList(
    tags$div(
      title,
      style = "display: inline-block; font-weight: bold; padding:0px;"
    ),

    tags$div(
      style = paste0("display: inline-block;", db_style),
      
      shinyWidgets::dropdownButton(
        size = "xs",
        label = "params",
        right = TRUE,
        icon = icon("sliders"),
        inline = TRUE,
        width = "50px",
        circle = FALSE,
        ...
      )
    )
  )
}

