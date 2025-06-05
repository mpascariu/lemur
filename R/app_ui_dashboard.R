# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Thu Jun  5 20:01:11 2025
# ------------------------------------------------- #


#' UI - dashboard page
#' @keywords internal
#' @export
ui_dashbord <- function() {
  
  layout_sidebar(
    style = "overflow-y: hidden;",
    sidebar = sidebar(
      width = "400px",
      div(
        style = "max-height: 90vh; overflow-y: auto;",  
        side_panel()
      )
    ),
    tagList(
      top_panel(),
      main_panel()
    )
  )
}


#' TOP PANEL
#' @keywords internal
top_panel <- function() {
  layout_columns(
    col_widths = c(4, 5, 2, 1),
    style = "align-items: top; max-height: 90vh; overflow-y: auto;",
    
    # Sex selection (conditionally shown)
    tagList(
      conditionalPanel(
        condition = "input.mode != 'mode_sex'",
        
        tags$style(HTML("
        .btn-silver {
        background-color: black;
        color: white;
        }
        .btn-silver.active, .btn-silver:active, .btn-silver:focus {
        background-color: black;
        color: white;
        }
          .btn-group, .btn-group-justified {
    flex-wrap: nowrap !important;
    overflow-x: auto;
  }")),
        
        shinyWidgets::radioGroupButtons(
          inputId = "sex",
          label = "Sex",
          choices = c(
            "Female" = "female",
            "Male"   = "male",
            "Both"   = "both"
          ),
          status = 'silver',
          selected = "both",
          justified = TRUE,
          size = "sm",
          width = "100%", 
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        )
      )
    ),
    
    # Mode selection
    tagList(
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
        status = 'silver',
        selected = "mode_cod",
        justified = TRUE,
        size = "sm",
        width = "100%",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      )
    ),
    
    # Percentage switch
    tagList(
      shinyWidgets::radioGroupButtons(
        inputId = "perc",
        label = "Data Type",
        choices = c(
          "Relative" = TRUE,
          "Absolute values" = FALSE
        ),
        status = "silver",
        selected = FALSE,
        justified = TRUE,
        size = "sm",
        width = "100%",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      )
    ),
    
    # Reset button
    tagList(
      tags$h5("Reset", style = "margin: 0px; padding: 0px; font-size: 14px;"),
      shinyWidgets::circleButton(
        inputId = "reset",
        style = "material-flat",
        size = "sm",
        status = "silver",
        icon = icon("recycle")
      )
    )
  )
}



#' SIDE PANEL
#' @keywords internal
side_panel <- function() {
  tagList(
    # Initialize Bootstrap 5 tooltips
    tags$script(HTML("
      $(function () {
        $('[data-bs-toggle=\"tooltip\"]').tooltip()
      });
    ")),
    
    # Container div with styles to prevent horizontal scrolling
    tags$div(
      style = "max-height: 90vh; overflow-y: auto; overflow-x: hidden; width: 100%; box-sizing: border-box;",
      
      # Region selection
      layout_columns(
        col_widths = 12,
        selectInput(
          inputId  = "region1",
          label    = "Region",
          choices  = list(
            Regions = lemur::data_app_input$regions, 
            Countries = lemur::data_app_input$countries
          ),
          selected = "GLOBAL",
          width    = "90%"
        ),
        conditionalPanel(
          condition = "input.mode == 'mode_cntr'",
          selectInput(
            inputId  = "region2",
            label    = "Region 2",
            choices  = list(
              Regions = lemur::data_app_input$regions, 
              Countries = lemur::data_app_input$countries
            ),
            selected = "EUROPE",
            width    = "90%"
          )
        )
      ),
      
      # Year slider with tooltip
      tags$div(
        style = "margin-bottom: 10px;",
        sliderTextInput(
          inputId = "time_slider",
          label = tags$span(
            "Year",
            `data-bs-toggle` = "tooltip",
            `data-bs-placement` = "right",
            title = "Select the year for which the data to correspond to"
          ),
          choices = lemur::data_app_input$period,
          selected = 2021,
          grid = TRUE,
          width = "90%"  # Ensure slider fills container
        )
      ),
      
      chooseSliderSkin("Flat"),
      setSliderColor_(rep("black", 50), c(1:50)),
      
      # Conditional panel for non-SDG modes
      conditionalPanel(
        condition = "input.mode !== 'mode_sdg' && input.mode !== 'mode_sdg2'",
        tagList(
          tags$div(
            style = "margin-bottom: 10px;",
            slider_input_(
              inputId = "cod_change",
              label = tags$span(
                "Modify the cause-specific risk of dying:",
                `data-bs-toggle` = "tooltip",
                `data-bs-placement` = "right",
                title = "Apply a percentage increase or decrease (%) of the risk selected below"
              ),
              value = -10
            )
          ),
          tags$div(
            style = "margin-bottom: 10px;",
            tags$div(
              style = "margin-bottom: 10px;",
              sliderTextInput(
                inputId = "age_change",
                label = tags$span(
                  "Age range:",
                  `data-bs-toggle` = "tooltip",
                  `data-bs-placement` = "right",
                  title = "On which age interval to change the risks? The ages outside the selected interval will not be affected."
                ),
                choices = lemur::data_app_input$x,
                selected = c(0, 110),
                grid = TRUE,
                width = "90%"  # Ensure slider fills container
              )
            )
          ),
          layout_columns(
            col_widths = 12,
            prettyCheckboxGroup(
              inputId = "cod_target",
              label = tags$span(
                "Cause of death:",
                `data-bs-toggle` = "tooltip",
                `data-bs-placement` = "right",
                title = paste(
                  "Which causes of death to be affected? ",
                  "The unchecked causes of death will maintain their absolute mortality impact."
                )
              ),
              choices = as.character(lemur::data_app_input$cause_name),
              selected = lemur::data_app_input$cause_name,
              icon = icon("check"),
              status = "success",
              animation = "rotate",
              outline = TRUE,
              inline = FALSE,
              width = "100%"  # Ensure checkboxes fill container
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
          )
        )
      ),
      
      # SDG mode
      conditionalPanel(
        condition = "input.mode == 'mode_sdg'",
        slider_input_(inputId = "sdg_3", label = "AIDS epidemic, tuberculosis, malaria and neglected tropical diseases:"),
        slider_input_(inputId = "sdg_4", label = "Mortality rate attributed to cardiovascular disease, cancer, diabetes or chronic respiratory disease:"),
        slider_input_(inputId = "sdg_1", label = "Under-five mortality rate:"),
        slider_input_(inputId = "sdg_2a", label = "Maternal mortality ratio:"),
        slider_input_(inputId = "sdg_2b", label = "Neonatal mortality rate:"),
        slider_input_(inputId = "sdg_5", label = "Suicide mortality rate:"),
        slider_input_(inputId = "sdg_6", label = "Death rate due to road traffic injuries:"),
        slider_input_(inputId = "sdg_7", label = "Mortality due to natural disasters:")
      ),
      
      # SDG2 mode
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
        slider_input_(inputId = "sdg2_21", label   = "Tuberculosis:")
      )
    )
  )
}



#' @keywords internal
main_panel <- function() {
  tagList(
    div(
      style = "align-items: top; margin-top: -5px; margin-bottom: -25px;",
      layout_columns(
        col_widths = c(7, 5),
        gap = "4px",
        chart_1(),
        chart_2()
      )
    ),
    
    div(
      style = "align-items: top; margin-top: -20px; margin-bottom: -25px;",
      layout_columns(
        col_widths = c(6, 6),
        gap = "4px",
        chart_3(),
        chart_4()
      )
    )
  )
}






#' @keywords internal
chart_1 <- function(height_ = 1) {
  layout_columns(
    width = 12, # Fill the parent column
    style = 'padding:0px 0px 0px 18px;',
    boxFrame(
      style = 'padding:0px',
      title = tags$div(
        "World Map",
        style = "display: inline-block; font-weight: bold; padding:0px;"
      ),
      leafletOutput(
        outputId = "figure1",
        height = "41.5vh"
      )
    )
  )
}


#' @keywords internal
chart_2 <- function() {
  layout_columns(
    width = 12, # Fills the parent column
    style = 'padding:0px;',
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
chart_3 <- function() {
  layout_columns(
    width = 12, # Fills the parent column (parent layout controls actual width)
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
            yes = tags$i(class = "fa fa-circle", style = "color: black"),
            no = tags$i(class = "fa fa-circle-o")
          ),
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
chart_4 <- function() {
  layout_columns(
    width = 12, # Fills the parent column; parent controls actual width
    style = 'padding:0px;',
    boxFrame(
      title = boxTitleInput(
        title = "Cause of Death / Age Decomposition of the Change in Life Expectancy at Birth",
        db_style = "padding: 0px 0px 0px 410px;",
        radioGroupButtons(
          inputId = "fig4_dim",
          label = "View by:",
          choices = c(
            "Age-and-COD" = "both",
            "Age" = "age",
            "COD" = "cod"
          ),
          justified = TRUE,
          checkIcon = list(
            yes = tags$i(class = "fa fa-circle", style = "color: black"),
            no = tags$i(class = "fa fa-circle-o")
          ),
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
    step    = step,
    width   = "90%",
  )
}



