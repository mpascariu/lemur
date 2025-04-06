# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Sun Apr  6 21:23:38 2025
# ------------------------------------------------- #


#' UI - dashboard page
#' @keywords internal
#' @export
ui_dashbord <- function() {
  
  tagList(
    # Add CSS to adjust the plot container size to half the screen
    tags$style(HTML("
    #plot {
      width: 50%;   # Half the width of the screen
      height: 50vh; # Half the height of the viewport
      margin: auto; # Center it on the screen
    }
  ")),
    
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
      width = 4,

      conditionalPanel(
        # If the we are in the 'mode_sex' we always do comparisons between 
        # male and females, therefore these 2 sexes need to be selected and 
        # present in the data at all times.
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
      
      shinyBS::bsTooltip(
        id = "sex",
        title = paste(
          "Select the female, male or entire population",
          "for which to display statistics."),
      ),
    ),

    column(
      width = 6,
      shinyWidgets::radioGroupButtons(
        inputId = "mode",
        label   = "Life Expectancy Comparisons",
        choices = c(
          "WITHIN REGION"   = "mode_cod",
          "BETWEEN REGIONS" = "mode_cntr",
          "SEX-GAP"         = "mode_sex",
          "SDG"             = "mode_sdg",
          "SDG2"            = "mode_sdg2"
          ),
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
      width = 1,
      style = 'padding-right:0px; margin-right: 0px;',
      tags$div(
        style = "padding: 25px 0px 0px 0px; margin-right: 0px;"
      ),
      #bookmarkButton(),
      actionButton(
        inputId = "reset",
        label   = "Reset",
        icon    = icon("recycle")
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
      selected = 2021,
      grid = TRUE
    ),
    shinyBS::bsTooltip(
      id = "time_slider",
      title = "Select the year for which the data to correspond to",
    ),

    chooseSliderSkin("Flat"),
    setSliderColor_(rep("black", 50), c(1:50)),

    conditionalPanel(
      condition = "input.mode !== 'mode_sdg' && input.mode !== 'mode_sdg2'",
      slider_input_(
        inputId = "cod_change",
        label = "Modify the cause-specific risk of dying:",
        value = -10
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
          width = 12,
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

          actionButton(
            inputId = "cod_target_all",
            label = "ALL",
            style = "width:48%;"
          ),
          actionButton(
            inputId = "cod_target_none",
            label = "NONE",
            style = "width:48%;"
          )
      ),
    ),

    # Side panel for sdg mode
    conditionalPanel(
      condition = "input.mode == 'mode_sdg'",
      
      #
      slider_input_(inputId = "sdg_3", label = "AIDS epidemic, tuberculosis, malaria and neglected tropical diseases:"),
      # End Epidemics. Goal: -100% relative to 2015 level
      slider_input_(inputId = "sdg_4", label = "Mortality rate attributed to cardiovascular disease, cancer, diabetes or chronic respiratory disease:"),
      #Goal: - 33.3% relative to 2015 level
      slider_input_(inputId = "sdg_1", label = "Under-five mortality rate:"),
      #
      slider_input_(inputId = "sdg_2a", label = "Maternal mortality ratio:"),
      # goal 25
      slider_input_(inputId = "sdg_2b", label = "Neonatal mortality rate:"),
      # Goal: -50% relative to 2015 level
      slider_input_(inputId = "sdg_5", label = "Suicide mortality rate:"),
      # Goal: -50% relative to 2015 level
      slider_input_(inputId = "sdg_6", label = "Death rate due to road traffic injuries:"),
      # substantially reduce the number of deaths from pollution
      slider_input_(inputId = "sdg_7", label = "Mortality due to natural disasters:"),

    ),
    
    # ******************************************************************
    # Side panel for sdg mode
    conditionalPanel(
      condition = "input.mode == 'mode_sdg2'",
      slider_input_(inputId = "sdg2_1", label   = "Cardiovascular Diseases:"),
      slider_input_(inputId = "sdg2_2", label   = "Chronic Respiratory diseases:"),
      slider_input_(inputId = "sdg2_3", label   = "Diabetes mellitus:"),
      slider_input_(inputId = "sdg2_4", label   = "Enteric Infections:"),
      slider_input_(inputId = "sdg2_5", label   = "Exposure to forces of nature:"),
      slider_input_(inputId = "sdg2_6", label   = "HIV/ AIDS / STD:"),
      slider_input_(inputId = "sdg2_7", label   = "Injuries (excl. Poisonings):"),
      slider_input_(inputId = "sdg2_8", label   = "Interpersonal Violence:"),
      slider_input_(inputId = "sdg2_9", label   = "Kidney disease (excl. Diabetes):"),
      slider_input_(inputId = "sdg2_10", label   = "Malaria:"),
      slider_input_(inputId = "sdg2_11", label   = "Maternal disorders:"),
      slider_input_(inputId = "sdg2_12", label   = "Neglected tropical diseases (excl. Malaria):"),
      slider_input_(inputId = "sdg2_13", label   = "Neonatal disorders:"),
      slider_input_(inputId = "sdg2_14", label   = "Neoplasms:"),
      slider_input_(inputId = "sdg2_15", label   = "Other Communicable:"),
      slider_input_(inputId = "sdg2_16", label   = "Other Non-Communicable:"),
      slider_input_(inputId = "sdg2_17", label   = "Poisonings:"),
      slider_input_(inputId = "sdg2_18", label   = "Respiratory Infections (excl. Tuberculosis):"),
      slider_input_(inputId = "sdg2_19", label   = "Self-harm:"),
      slider_input_(inputId = "sdg2_20", label   = "Transport injuries:"),
      slider_input_(inputId = "sdg2_21", label   = "Tuberculosis:"),
    ),
    # ******************************************************************
    
    # radioGroupButtons(
    #   inputId = "stack_charts",
    #   label   = "Stack Charts?!",
    #   choices = c("NO", "YES"),
    #   justified = TRUE
    # )
  )
}


#' @keywords internal
main_panel <- function() {
  
  # tagList(
  #   conditionalPanel(
  #     condition = "input.stack_charts == 'NO'",
      tagList(
        chart_1(width_ = 7),
        chart_2(width_ = 5),
        chart_3(width_ = 6),
        chart_4(width_ = 6)
        )
  #     )
  #   ,
  # 
  #   conditionalPanel(
  #     condition = "input.stack_charts == 'YES'",
  #     tagList(
  #       fluidRow(chart_1(width_ = 10, height_ = 1.0, offset_ = 1)),
  #       fluidRow(chart_2(width_ = 10, height_ = 1.1, offset_ = 1)),
  #       fluidRow(chart_3(width_ = 10, height_ = 1.3, offset_ = 1)),
  #       fluidRow(chart_4(width_ = 10, height_ = 1.3, offset_ = 1))
  #     )
  #   )
  # )
}


#' @keywords internal
chart_1 <- function(width_, height_ = 1, offset_ = 0) {
  column(
    width = width_,
    style = 'padding:0px 0px 0px 18px;',
    offset = offset_,
    
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
        height = "41.5vh"
      )
    )
  )
}

#' @keywords internal
chart_2 <- function(width_, offset_ = 0) {
  column(
    width = width_,
    style = 'padding:0px;',
    offset = offset_,
    
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
        height = "40vh"
      )
    )
  )
}

#' @keywords internal
chart_3 <- function(width_, offset_ = 0) {
  column(
    width = width_,
    style = 'padding-right:0px; padding-top:0px; padding-bottom:0px',
    offset = offset_,
    
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
        height = "38vh"
      )
    )
  )
}

#' @keywords internal
chart_4 <- function(width_, offset_ = 0) {
  column(
    width = width_,
    style = 'padding:0px;',
    offset = offset_,
    
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
        height = "38vh"
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

#' @keywords internal
slider_input_ <- function(
    inputId, 
    label, 
    post    = "%",
    value   = 0,
    min     = -100,
    max     = 100,
    step    = 1
    ) {
  sliderInput(
    inputId = inputId,
    label   = label,
    post    = post,
    value   = value,
    min     = min,
    max     = max,
    step    = step
  )
}



