# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-08 18:17:53
# --------------------------------------------

#' UI - dashboard page
#' @keywords internal
#' @export
ui_dashbord <- function() {
  layout_sidebar(
    style = "background-color: #fff;",
    sidebar = sidebar(
      side_panel(),
      width = "400px",
    ),
    # Main content area: a vertical flex column that fills the page height,
    # so the chart rows in main_panel() stretch to fill the window (no unused
    # space at the bottom) and resize together with it.
    div(
      class = "html-fill-item html-fill-container",
      style = "display: flex; flex-direction: column;",
      # Loading overlay. position:fixed so it covers the viewport (and stays
      # out of the flex flow). The server hides it via the hideLoading custom
      # message once the UI is ready (see app_server.R). The main tables now
      # load in ~1s, but the overlay keeps the brief gap from looking like a
      # frozen page on slow machines.
      tags$div(
        id = "lemur-loading",
        style = "position:fixed; top:0; left:0; width:100%; height:100%;
                 background:#fff; z-index:9999; display:flex;
                 align-items:center; justify-content:center;
                 font-family:'Roboto Condensed',sans-serif;",
        tags$style("
          .lemur-spinner {
            width: 42px; height: 42px; margin: 0 auto 14px;
            border: 4px solid #e9ecef; border-top-color: #0d6efd;
            border-radius: 50%; animation: lemur-spin 0.9s linear infinite;
          }
          @keyframes lemur-spin { to { transform: rotate(360deg); } }
        "),
        tags$div(
          style = "text-align:center; color:#495057; font-size:1.2rem;",
          tags$div(class = "lemur-spinner"),
          "Loading data\u2026"
        )
      ),
      top_panel(),
      main_panel()
    )
  )
}



#' TOP PANEL
#' @keywords internal
top_panel <- function() {
  layout_columns(
    col_widths = breakpoints(
      # Large screens: custom widths (out of 12)
      lg = c(4, 5, 2, 1),
      # Medium and small screens: stack all vertically
      md = c(12, 12, 12, 12),
      sm = c(12, 12, 12, 12)
    ),
    # Keep the control row at its natural (content) height instead of
    # stretching it to fill the page alongside the charts.
    fill = FALSE,

    # Sex selection (conditionally shown)
    card(
      class = "border-0",
      conditionalPanel(
        condition = "input.mode != 'mode_sex'",
        
        tags$style(HTML("
        .btn-bw {
        background-color: white;
        color: black;
        }
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
        }
        ")),
        
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
    card(
      class = "border-0",
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
    card(
      class = "border-0",
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
    card(
      class = "border-0",
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
    
    tags$div(
      style = "overflow-y: hidden; overflow-x: hidden; width: 100%; box-sizing: border-box;",
      
      # Region selection
      layout_columns(
        col_widths = 12,
        selectInput(
          inputId  = "region1",
          label    = "Region",
          choices  = list(
            Regions = data_app_input$regions, 
            Countries = data_app_input$countries
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
              Regions = data_app_input$regions, 
              Countries = data_app_input$countries
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
          choices = data_app_input$period,
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
                choices = data_app_input$x,
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
              choices = as.character(data_app_input$cause_name),
              selected = data_app_input$cause_name,
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
  # Two chart rows stacked in a fillable column. Each row takes half of the
  # remaining height and its cards stretch to fill it, so the charts resize
  # with the window. On short/small screens each row keeps a minimum height
  # and the panel scrolls instead of crushing the charts.
  div(
    class = "html-fill-item html-fill-container",
    style = "display: flex; flex-direction: column; overflow-y: auto;",
    div(
      class = "html-fill-item html-fill-container",
      style = "flex: 1 1 0; min-height: 320px;",
      layout_columns(
        col_widths = breakpoints(
          lg = c(7, 5),
          md = c(12, 12),
          sm = c(12, 12)
        ),
        gap = "4px",
        chart_1(),
        chart_2()
      )
    ),
    div(
      class = "html-fill-item html-fill-container",
      style = "flex: 1 1 0; min-height: 320px;",
      layout_columns(
        col_widths = breakpoints(
          lg = c(6, 6),
          md = c(12, 12),
          sm = c(12, 12)
        ),
        gap = "4px",
        chart_3(),
        chart_4()
      )
    )
  )
}


#' @keywords internal
boxTitleInput2 <- function(title, ...) {
  card_header(
    style = "background-color: #fff; border-bottom: none;",
    tags$div(
      style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
      tags$span(title, style = "font-weight: bold; font-size: 1.1rem;"),
      
      shinyWidgets::dropdownButton(
        size   = "sm",
        label  = "",
        icon   = icon("sliders-h"),
        inline = TRUE,
        width  = "180px",
        circle = FALSE,
        status = "bw",
        ...
      )
    )
  )
}


#' @keywords internal
chart_1 <- function(height_ = 1) {
  card(
    class = "border-0",
    fill = TRUE,
    card_header(
      style = "background-color: #fff; border-bottom: none;",
      tags$div(
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span("World Map", style = "font-weight: bold; font-size: 1.1rem;")
        )
    ),
    card_body(
      fill = TRUE,
      leafletOutput(outputId = "figure1", height = "100%")
      )
  )
}



#' @keywords internal
chart_2 <- function() {
  card(
    class = "border-0",
    fill = TRUE,
    boxTitleInput2(
      title = "Difference in Life Expectancy at various ages",
      selectInput(
        inputId = "fig2_x",
        label = "Ages to be displayed",
        choices = data_app_input$x,
        selected = seq(0, 110, 10),
        multiple = TRUE
      )
    ),
    card_body(
      fill = TRUE,
      plotlyOutput(outputId = "figure2", height = "100%")
    )
  )
}

#' @keywords internal
chart_3 <- function() {
  card(
    class = "border-0",
    fill = TRUE,
    boxTitleInput2(
      title = "Cause of Death Distribution",
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
    card_body(
      fill = TRUE,
      plotlyOutput(outputId = "figure3", height = "100%")
    )
  )
}


#' @keywords internal
chart_4 <- function() {
  card(
    class = "border-0",
    fill = TRUE,
    boxTitleInput2(
      title = "Cause of Death / Age Decomposition of the Change in Life Expectancy at Birth",
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
    card_body(
      fill = TRUE,
      plotlyOutput(outputId = "figure4", height = "100%")
    )
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



