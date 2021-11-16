# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Tue Nov 16 23:43:28 2021
# --------------------------------------------------- #

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @keywords internal
#' @export
app_server <- function(input, output, session) {

  # INPUT DATA
  # cod data ---
  data_cod <- reactive({
    if (input$mode != "mode_sdg") {
      dt_filter(data_gbd2019_cod,
                input$mode,
                input$region1,
                input$region2,
                input$sex,
                input$time_slider
                )
    }
  })

  # sdg data ---
  data_sdg <- reactive({
    if (input$mode == "mode_sdg") {
      dt_filter(data_gbd2019_sdg,
                input$mode,
                input$region1,
                input$region2,
                input$sex,
                input$time_slider
                )
    }
  })

  # life tables data
  data_lt  <- reactive({
    dt_filter(data_gbd2019_lt,
              input$mode,
              input$region1,
              input$region2,
              input$sex,
              input$time_slider
              )
    })

  # Reduction matrix
  data_cod_change <- reactive({

    if (input$mode != "mode_sdg") {

      M <- build_reduction_matrix(
        data = data_cod(),
        select_cod = input$cod_target,
        select_x   = input$age_change,
        cod_change = input$cod_change
      )

    } else {
      M <- build_reduction_matrix(
            data       = data_sdg(),
            select_cod = as.character(unique(data_sdg()$cause_name)),
            select_x   = 0:110,
            cod_change = 0
          )

      S1 = paste(0:1)
      S3 = c("HIV/ AIDS / STD",
             "Tuberculosis",
             "Malaria",
             "Neglected Tropical Diseases (excl. Malaria)")
      S4 = c("Cardiovascular Diseases",
             "Neoplasms",
             "Diabetes",
             "Chronic Respiratory Diseases")
      S5 = "Self-Harm"
      S6 = "Transport Injuries"
      S7 = "Exposure to Forces of Nature"

      M[S1,   ] <- input$sdg_1
      M[  , S3] <- input$sdg_3
      M[  , S4] <- input$sdg_4
      M[  , S5] <- input$sdg_5
      M[  , S6] <- input$sdg_6
      M[  , S7] <- input$sdg_7
    }

    M
  })

  # Prepare data for figures depending on with mode is selected
  data_fig <- reactive({

    switch (
      input$mode,

      mode_cod  = prepare_data_mode_cod(
        cod        = data_cod(),
        lt         = data_lt(),
        region1    = input$region1,
        cod_change = data_cod_change()
      ),

      mode_cntr = prepare_data_mode_cntr(
        cod        = data_cod(),
        lt         = data_lt(),
        region1    = input$region1,
        region2    = input$region2,
        cod_change = data_cod_change()
        ),

      mode_sex = prepare_data_mode_sex(
        cod        = data_cod(),
        lt         = data_lt(),
        region1    = input$region1,
        cod_change = data_cod_change()
        ),

      mode_sdg = prepare_data_mode_sdg(
        cod        = data_sdg(),
        lt         = data_lt(),
        region     = input$region1,
        cod_change = data_cod_change()
        )
    )
  })

  # Decompose the difference in life expectancy at birth
  data_decomp <- reactive({
    with(data_fig(),
         decompose_by_cod(
           lt_initial,
           lt_final,
           cod_initial,
           cod_final
           )
         )
  })

  # ----------------------------------------------------------------------------
  # RENDER FIGURES

  # Figure 1 - The Map
  output$figure1 <- renderLeaflet(
    suppressWarnings(
      plot_map(location = input$region1)
    )
  )

  # Figure 2 - The change
  output$figure2 <- renderPlotly({
    xlab <- if (input$perc) {
      "Difference in Life Expectancy [%]"
    } else {
      "Difference in Life Expectancy (years)"
    }

    p2 <- plot_change(
      L1 = data_fig()$lt_final,
      L2 = data_fig()$lt_initial,
      age = input$fig2_x,
      perc = input$perc) +
      geom_point(size = 4) +
      labs(x = "", y = "") +
      theme(
        axis.text = element_text(size = 10)
      )

    p2 <- ggplotly(p2, tooltip = c("x", "y")) %>%
      plotly::layout(
        xaxis = list(title = xlab),
        yaxis = list(title = "Age (years)")) %>%
      plotly::layout(
        xaxis = list(titlefont = list(size = 14), tickfont = list(size = 11)),
        yaxis = list(titlefont = list(size = 14), tickfont = list(size = 11)))

    p2

  })

  # Figure 3 - The COD Distribution
  output$figure3 <- renderPlotly({

    xlab <- if (input$perc) {
      "Proportion of the Total No. of Deaths\n[%]"
    } else {
      "Number of Deaths\n"
    }

    if (input$mode == "mode_cod") {
      p <- plot_cod(
        cod  = data_fig()$cod_final,
        perc = input$perc,
        type = "barplot")

    } else if (input$mode == "mode_cntr") {
      cod <- bind_rows(
        data_fig()$cod_initial,
        data_fig()$cod_final
        )

      p <- plot_cod(
        cod  = cod,
        perc = input$perc,
        type = "barplot") +
        facet_wrap("region")

    } else if (input$mode == "mode_sex") {
      cod <- bind_rows(
        data_fig()$cod_initial,
        data_fig()$cod_final
        )

      p <- plot_cod(
        cod  = cod,
        perc = input$perc,
        type = "barplot") +
        facet_wrap("sex")

    } else if (input$mode == "mode_sdg") {
      p <- plot_cod(
        cod  = data_fig()$cod_final,
        perc = input$perc,
        type = "barplot")
    }

    p <- p +
      labs(x = "", y = "") +
      theme(
        axis.text = element_text(size = 7)
      )

    p <- ggplotly(p, tooltip = c("fill", "x")) %>%
      plotly::layout(
        xaxis = list(title = xlab),
        yaxis = list(title = 'Causes of Death')) %>%
      plotly::layout(
        xaxis = list(titlefont = list(size = 14), tickfont = list(size = 11)),
        yaxis = list(titlefont = list(size = 14), tickfont = list(size = 11)))

    p
  })

  # Figure 4 - The Decomposition
  output$figure4 <- renderPlotly({

    # compute % is necessary
    if (input$perc & input$fig4_dim != "cod") {
      ttip <- c("fill", "y")
      ylab <- "Change in Life Expectancy at Birth\n[%]"
      xlab <- "Age Group (years)"

    } else if (!input$perc & input$fig4_dim != "cod"){
      ttip <- c("fill", "y")
      ylab <- "Change in Life Expectancy at Birth\n(years)"
      xlab <- "Age Group (years)"

    } else if (input$perc & input$fig4_dim == "cod"){
      ttip = c("fill", "x")
      ylab <- "Causes of Death"
      xlab <- "Change in Life Expectancy at Birth [%]"

    } else if (!input$perc & input$fig4_dim == "cod"){
      ttip = c("fill", "x")
      ylab <- "Causes of Death"
      xlab <- "Change in Life Expectancy at Birth [years]"
    }


    p4 <- plot_decompose(
      object = data_decomp(),
      perc   = input$perc,
      by     = input$fig4_dim)

    p4 <- ggplotly(p4, tooltip = ttip) %>%
      plotly::layout(
        xaxis = list(title = xlab),
        yaxis = list(title = ylab)) %>%
      plotly::layout(
        xaxis = list(titlefont = list(size = 14), tickfont = list(size = 11)),
        yaxis = list(titlefont = list(size = 14), tickfont = list(size = 11)))

    p4
  })

  # ----------------------------------------------------------------------------
  # EVENTS
  observeEvent(input$cod_target_all, {
    updatePrettyCheckboxGroup(
      session,
      inputId = "cod_target",
      selected = levels(data_cod()$cause_name)
    )
  })

  observeEvent(input$cod_target_none, {
    updatePrettyCheckboxGroup(
      session,
      inputId = "cod_target",
      selected = "none"
    )
  })

}


#' @keywords internal
dt_filter <- function(data, mode, region1, region2, gender, year) {

  region = period = sex <- NULL

  # we use data.table method to filter here because is faster
  # and we will do this all a lot
  dt <- as.data.table(data)
  dt <- dt[period == year & region %in% c(region1, region2)]

  if (mode != "mode_sex") {
    dt <- dt[sex == gender]
  }

  return(as_tibble(dt))
}
