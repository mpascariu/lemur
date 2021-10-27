# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Thu Oct 28 00:07:44 2021
# --------------------------------------------------- #

#' @keywords internal
top_panel <- function() {
  fluidRow(
    
    column(
      width = 3,
      
      conditionalPanel(
        condition = "input.mode != 'mode_sex'",
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
        ),
      ),
      
      conditionalPanel(
        condition = "input.mode == 'mode_sex'",
        shinyWidgets::checkboxGroupButtons(
          inputId = "sex",
          label   = "Sex",
          choices = c(
            "Female" = "female", 
            "Male"   = "male",
            "Both"   = "both"),
          selected = c("female", "male"),
          justified = TRUE,
          size = "sm",
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        ),
      ),
      bsTooltip(
        id = "sex", 
        title = paste(
          "Select the female, male or entire population",
          "for which to display statistics."),
      ),
    ),
    
    
    column(
      width = 4,
      shinyWidgets::radioGroupButtons(
        inputId = "mode",
        label   = "Dashboard Mode",
        choices = c(
          "COD Risk Change" = "mode_cod", 
          "Compare Regions" = "mode_cntr",
          "Sex Comparison" = "mode_sex",
          "SDGs" = "mode_sdg"),
        selected = "mode_cntr",
        justified = TRUE,
        size = "sm",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      ),
      bsTooltip(
        id = "mode", 
        title = "Select the mode in which the dashboard to operate.",
        placement = "bottom"
      )
    ),
    
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
      condition = "input.mode != 'mode_cntr'",
      selectInput(
        inputId  = "region1",
        label    = "Region",
        choices  = MortalityCauses::data_app_input$region,
        selected = "Romania",
        width    = "100%"
      ), 
    ),
    
    conditionalPanel(
      condition = "input.mode == 'mode_cntr'",
      fluidRow(
        column(
          width = 6,
          selectInput(
            inputId  = "region1",
            label    = "Region",
            choices  = MortalityCauses::data_app_input$region,
            selected = "Romania",
            width    = "100%"
          ) 
        ),
        
        column(
          width = 6,
          selectInput(
            inputId  = "region2",
            label    = "Region 2",
            choices  = MortalityCauses::data_app_input$region,
            selected = "Mexico",
            width    = "100%",
          )
        )
      )
    ),
    
    sliderTextInput(
      inputId = "time_slider",
      label = "Year",
      choices = MortalityCauses::data_app_input$period,
      selected = 2019,
      grid = TRUE
    ),
    bsTooltip(
      id = "time_slider", 
      title = "Select the year for which the data to correspond to",
    ),
    
    chooseSliderSkin("Flat"),
    setSliderColor(rep("black", 10), c(1:10)),
    
    conditionalPanel(
      condition = "input.mode != 'mode_sdg'",
      sliderInput(
        inputId = "cod_change",
        label = "Modify the case-specific risk of dying:",
        post = "%",
        value = 0,
        min = -100,
        max = 100,
        step = 5
      ),
      bsTooltip(
        id = "cod_change", 
        title = paste(
          "Apply a percentage increase or decrease (%)",
          " of the risk selected below"),
      ),
      
      sliderTextInput(
        inputId = "age_change",
        label = "Age range:",
        choices = MortalityCauses::data_app_input$x,
        selected = c(0, 110),
        grid = TRUE
      ),
      bsTooltip(
        id = "age_change", 
        title = paste(
          "On which age interval to change the risks? ",
          "The ages outside the selected interval will not be affected."),
      ),
      
      fluidRow(
        column(
          width = 10,
          prettyCheckboxGroup(
            inputId = "cod_target",
            label = "Cause of death:", 
            choices = sort(MortalityCauses::data_app_input$cause_name),
            selected = MortalityCauses::data_app_input$cause_name,
            icon = icon("check"),
            status = "success",
            animation = "rotate",
            outline = TRUE,
            inline = FALSE
          ),
          bsTooltip(
            id = "cod_target", 
            title = paste(
              "Which causes of death to be affected? ",
              "The unchecked causes of death will maintain",
              "their absolute mortality impact."),
            placement = "top",
          ),
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
    ),
    
    # Side panel for sdg mode
    conditionalPanel(
      condition = "input.mode == 'mode_sdg'",
      
      sliderInput(
        inputId = "goal_1_maternal",
        label = "Maternal mortality ratio:",
        post = " per 100k",
        value = 80,
        min = 0,
        max = 200,
        step = 1
      ),
      
      sliderInput(
        inputId = "goal_2_underfive",
        label = "Under-five mortality rate:",
        post = " per 1000 live births",
        value = 80,
        min = 0,
        max = 200,
        step = 1
      ),
      
      sliderInput( # goal 25
        inputId = "goal_3_neonatal",
        label = "Neonatal mortality rate:",
        post = " per 1000 live births",
        value = 80,
        min = 0,
        max = 200,
        step = 1
      ),
      
      sliderInput( # goal 0
        inputId = "goal_4_cardio",
        label = "Mortality rate attributed to cardiovascular disease, cancer, diabetes or chronic respiratory disease:",
        post = " per 1,000 uninfected",
        value = 80,
        min = 0,
        max = 200,
        step = 1
      ),
      
      sliderInput(
        inputId = "goal_5_suicide",
        label = "Suicide mortality rate:",
        post = " ",
        value = 80,
        min = 0,
        max = 200,
        step = 1
      ),
      
      sliderInput( # halve the number of global deaths on road traffic accidents
        inputId = "goal_6_road",
        label = "Death rate due to road traffic injuries:",
        post = " per 100k",
        value = 80,
        min = 0,
        max = 200,
        step = 1
      ),
      
      sliderInput( # substantially reduce the number of deaths from pollution
        inputId = "goal_7_road",
        label = "Mortality due to air pollution:",
        post = " per 100k",
        value = 80,
        min = 0,
        max = 200,
        step = 1
      ),
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
      style = "display: inline-block; font-weight: bold; padding:0px; margin: -20px 0px 0px 5px;",
      shinyWidgets::dropdownButton(
        size = "xs",
        label = "",
        right = TRUE,
        icon = icon("sliders"),
        inline = TRUE,
        width = "50px",
        circle = FALSE,
        ...
      )
    ),
    
    tags$div(
      # "subtitle....",
      style = "display: padding:0px; margin: 0px 0px -20px 5px; font-size: 12px;"
    ),
  )
}































