# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Thu Mar 17 18:15:40 2022
# --------------------------------------------------- #

#' UI - dashboard page
#' @keywords internal
#' @export
ui_dashbord <- function() {
  
  tagList(
    
    # # Disable the vertical scroll bar in shiny dashboard
    # tags$head(
    #   tags$style(
    #     "body {overflow-y: hidden;}"
    #   )
    # ),
    
    tagList(
      column(
        width = 2,
        side_panel()
      ),
      
      column(
        width = 10,
        top_panel(),
        main_panel()
      )
    )
  )
}


#' TOP PANEL
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
      shinyBS::bsTooltip(
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
        label   = "Life Expectancy Comparisons",
        choices = c(
          "WITHIN REGION"   = "mode_cod",
          "BETWEEN REGIONS" = "mode_cntr",
          "SEX-GAP"         = "mode_sex",
          "SDG"             = "mode_sdg"),
        selected = "mode_cod",
        justified = TRUE,
        size = "sm",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      ),
      shinyBS::bsTooltip(
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
        value = FALSE,
        onStatus = "success",
        offStatus = "danger",
        label = icon("percent"),
        size = "small"
      )
    ),

    column(
      width = 4,
      style = 'padding-right:0px; margin-right: 0px;',
      tags$div(
        style = "padding: 25px 0px 0px 0px; margin-right: 0px;"
      ),
      bookmarkButton(
        id = "bookmark",
        label = "Bookmark"
      ),
      actionButton(
        inputId = "reset",
        icon = icon("recycle"),
        label = "Reset Selection"
      ),
    ),
  )
}


#' SIDE PANEL
#' @keywords internal
side_panel <- function() {
  tagList(

      fluidRow(
        column(
          width = 12,
          selectInput(
            inputId  = "region1",
            label    = "Region",
            choices  = list(Regions = lemur::data_app_input$regions, 
                            Countries = lemur::data_app_input$countries),
            selected = "GLOBAL",
            width    = "100%"
          )
        ),
        conditionalPanel(
          condition = "input.mode == 'mode_cntr'",
          column(
            width = 12,
            selectInput(
              inputId  = "region2",
              label    = "Region 2",
              choices  = list(Regions = lemur::data_app_input$regions, 
                              Countries = lemur::data_app_input$countries),
              selected = "EUROPE",
              width    = "100%",
            )
          )
      )
    ),

    sliderTextInput(
      inputId = "time_slider",
      label = "Year",
      choices = lemur::data_app_input$period,
      selected = 2019,
      grid = TRUE
    ),
    shinyBS::bsTooltip(
      id = "time_slider",
      title = "Select the year for which the data to correspond to",
    ),

    chooseSliderSkin("Flat"),
    setSliderColor(rep("black", 10), c(1:10)),

    conditionalPanel(
      condition = "input.mode != 'mode_sdg'",
      sliderInput(
        inputId = "cod_change",
        label = "Modify the cause-specific risk of dying:",
        post = "%",
        value = -10,
        min = -100,
        max = 100,
        step = 5
      ),
      shinyBS::bsTooltip(
        id = "cod_change",
        title = paste(
          "Apply a percentage increase or decrease (%)",
          " of the risk selected below"),
      ),

      sliderTextInput(
        inputId = "age_change",
        label = "Age range:",
        choices = lemur::data_app_input$x,
        selected = c(0, 110),
        grid = TRUE
      ),
      shinyBS::bsTooltip(
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
            choices = as.character(lemur::data_app_input$cause_name),
            selected = lemur::data_app_input$cause_name,
            icon = icon("check"),
            status = "success",
            animation = "rotate",
            outline = TRUE,
            inline = FALSE
          ),
          shinyBS::bsTooltip(
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

      # sliderInput(
      #   inputId = "goal_1_maternal",
      #   label = "Maternal mortality ratio:",
      #   post = " per 100k",
      #   value = 80,
      #   min = 0,
      #   max = 200,
      #   step = 1
      # ),

      sliderInput(
        inputId = "sdg_1",
        label = "Under-five mortality rate:",
        # post = " per 1000 live births",
        post = "%",
        value = 0,
        min = -100,
        max = 100,
        step = 5
      ),

      # sliderInput( # goal 25
      #   inputId = "goal_3_neonatal",
      #   label = "Neonatal mortality rate:",
      #   post = " per 1000 live births",
      #   value = 80,
      #   min = 0,
      #   max = 200,
      #   step = 1
      # ),

      sliderInput(# End Epidemics. Goal: -100% relative to 2015 level
        inputId = "sdg_3",
        label   = "AIDS epidemic, tuberculosis, malaria and neglected tropical diseases:",
        post    = "%",
        value   = 0,
        min     = -100,
        max     = 100,
        step    = 5
      ),

      sliderInput( #Goal: - 33.3% relative to 2015 level
        inputId = "sdg_4",
        label   = "Mortality rate attributed to cardiovascular disease, cancer, diabetes or chronic respiratory disease:",
        post    = "%",
        value   = 0,
        min     = -100,
        max     = 100,
        step    = 5
      ),

      sliderInput( # Goal: -50% relative to 2015 level
        inputId = "sdg_5",
        label   = "Suicide mortality rate:",
        post    = "%",
        value   = 0,
        min     = -100,
        max     = 100,
        step    = 5
      ),

      sliderInput( # Goal: -50% relative to 2015 level
        inputId = "sdg_6",
        label   = "Death rate due to road traffic injuries:",
        post    = " %",
        value   = 0,
        min     = -100,
        max     = 100,
        step    = 5
      ),

      sliderInput( # substantially reduce the number of deaths from pollution
        inputId = "sdg_7",
        label   = "Mortality due to air pollution:",
        post    = "%",
        value   = 0,
        min     = -100,
        max     = 100,
        step    = 5
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
            title = "Difference in Life Expectancy at various ages",
            db_style = "padding: 0px 0px 0px 340px;",
            selectInput(
              inputId = "fig2_x",
              label = "Ages to be displayed",
              choices = lemur::data_app_input$x,
              selected = seq(0, 110, 10),
              multiple = TRUE
            )
          ),

          plotlyOutput(
            outputId = "figure2",
            height = 350*0.955
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
              choices = c("Bar-plot" = "barplot"),
              justified = TRUE,
              checkIcon = list(
                yes = tags$i(class = "fa fa-circle",
                             style = "color: black"),
                no = tags$i(class = "fa fa-circle-o")),
              direction = "vertical"
            )
          ),

          plotlyOutput(
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
            title = "Cause of Death / Age Decomposition of the Change in Life Expectancy at Birth",
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

          plotlyOutput(
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
  box(
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
        size   = "xs",
        label  = "",
        right  = TRUE,
        icon   = icon("sliders-h"),
        inline = TRUE,
        width  = "50px",
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































