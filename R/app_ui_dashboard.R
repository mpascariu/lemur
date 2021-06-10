# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Thu Jun 10 22:11:58 2021
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
          "Compare Regions" = "cntr_compare",
          "Sex Comparison" = "sex_compare"),
        selected = "cntr_compare",
        justified = TRUE,
        size = "sm",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      )
    ),
    
    # column(
    #    width = 8
    # ),
    
    column(
      width = 1,
      style = 'padding-right:0px; margin-right: 0px;',
      
      tags$div(
        style = "padding: 25px 0px 0px 0px; margin-right: 0px;"
      ), 
      switchInput(
        inputId = "perc",
        value = TRUE,
        onStatus = "success", 
        offStatus = "danger",
        label = icon("percent"),
        size = "small"
      )
    )
  )
}


#' @keywords internal
side_panel <- function() {
  tagList(
    conditionalPanel(
      condition = "input.mode != 'sex_compare'",
      shinyWidgets::radioGroupButtons(
        inputId = "sex",
        label   = "Sex",
        choices = c(
          "Female" = "female", 
          "Male"   = "male",
          "Both"   = "both"),
        selected = "both",
        justified = TRUE,
        size = "sm",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      )
    ),
    
    selectInput(
      inputId  = "region1",
      label    = "Region",
      choices  = MortalityCauses::data_app_input$region,
      selected = "Romania",
      width    = "100%"
    ), 
    
    conditionalPanel(
      condition = "input.mode == 'cntr_compare'",
      
      selectInput(
        inputId  = "region2",
        label    = "Region 2",
        choices  = MortalityCauses::data_app_input$region,
        selected = "Mexico",
        width    = "100%"
      )
    ),
    
    chooseSliderSkin("Flat"),
    setSliderColor(c("black", "black"), c(1, 2)),
    sliderInput(
      inputId = "cod_change",
      label = "Modify the case-specific risk of dying:",
      post = "%",
      value = 0,
      min = -100,
      max = 100,
      step = 5
    ),
    
    sliderTextInput(
      inputId = "age_change",
      label = "On which age interval to change the risks?",
      choices = MortalityCauses::data_app_input$x,
      selected = c(0, 110),
      grid = TRUE
    ),
    
    fluidRow(
      column(
        width = 10,
        prettyCheckboxGroup(
          inputId = "cod_target",
          label = "Which cause of death to be affected:", 
          choices = sort(MortalityCauses::data_app_input$cause_name),
          selected = MortalityCauses::data_app_input$cause_name,
          icon = icon("check"),
          status = "success",
          animation = "rotate",
          outline = TRUE,
          inline = FALSE
        )
      ),
      
      column(
        width = 2,
        style = 'padding:0px;',
        br(),
        actionButton(
          inputId = "cod_target_all", 
          label = "ALL",
          style = "width:100%;"
          ),
        actionButton(
          inputId = "cod_target_none", 
          label = "NONE",
          style = "width:100%;"
          )
        
      )
    )

  )
}


#' @keywords internal
main_panel <- function() {
  tagList(
    fluidRow(
      column(
        width = 7,
        style ='padding:0px 0px 0px 18px;',
        
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
            height = 381*0.93
          )
        )
      ),
      
      column(
        width = 5,
        style='padding:0px;',
        
        boxFrame(
          title = boxTitleInput(
            title = "Difference in Life Expectancy",
            db_style = "padding: 0px 0px 0px 340px;",
            selectInput(
              inputId = "fig2_x",
              label = "Ages to be displayed",
              choices = MortalityCauses::data_app_input$x,
              selected = seq(0, 100, 10),
              multiple = TRUE
            )
          ),
          
          plotOutput(
            outputId = "figure2",
            height = 343*0.93
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
            db_style = "padding: 0px 0px 0px 450px;",
            radioGroupButtons(
              inputId = "fig3_chart_type",
              label = "View by:",
              choices = c("Bar-plot" = "barplot", 
                          "Pie-chart" = "piechart"),
              justified = TRUE,
              checkIcon = list(
                yes = tags$i(class = "fa fa-circle", 
                             style = "color: black"),
                no = tags$i(class = "fa fa-circle-o")),
              direction = "vertical"
            )
          ),
          
          plotOutput(
            outputId = "figure3",
            height = 330
          )
        )
      ),
      
      column(
        width = 6,
        style ='padding:0px;',
        
        boxFrame(
          title = boxTitleInput(
            title = "Cause of Death / Age Decomposition",
            db_style = "padding: 0px 0px 0px 410px;",
            radioGroupButtons(
              inputId = "fig4_dim",
              label = "View by:",
              choices = c("Age-and-COD" = "both", 
                          "Age" = "age", 
                          "COD" = "cod"),
              justified = TRUE,
              checkIcon = list(
                yes = tags$i(class = "fa fa-circle", 
                             style = "color: black"),
                no = tags$i(class = "fa fa-circle-o")),
              direction = "vertical"
            )
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
      style = "display: inline-block; font-weight: bold; padding:0px; margin: -20px 0px 0px 5px;"
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
    ),
    
    tags$div(
      "subtitle....",
      style = "display: padding:0px; margin: 0px 0px -20px 5px; font-size: 12px;"
    ),
  )
}































