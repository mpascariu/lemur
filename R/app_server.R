# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Tue Mar 15 15:45:34 2022
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
      dt_filter(
        lemur::data_gbd2019_cod,
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
      dt_filter(
        lemur::data_gbd2019_sdg,
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
    dt_filter(
      lemur::data_gbd2019_lt,
      input$mode,
      input$region1,
      input$region2,
      input$sex,
      input$time_slider
      )
    })

  # Reduction matrix
  data_cod_change <- reactive({

    if (input$mode == "mode_sdg") {
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
      
      M[  , S3] <- input$sdg_3
      M[  , S4] <- input$sdg_4
      M[  , S5] <- input$sdg_5
      M[  , S6] <- input$sdg_6
      M[  , S7] <- input$sdg_7
      # M[S1,   ] <- input$sdg_1
      M[S1,   ] <- input$sdg_1 + M[S1, ] * abs(input$sdg_1)/100
      
    } else {
      M <- build_reduction_matrix(
        data = data_cod(),
        select_cod = input$cod_target,
        select_x   = input$age_change,
        cod_change = input$cod_change
      )
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

      mode_sdg = prepare_data_mode_cod(
        cod        = data_sdg(),
        lt         = data_lt(),
        region1    = input$region1,
        cod_change = data_cod_change()
        )
    )
  })
  
  # Decompose the difference in life expectancy at birth
  data_decomp <- reactive({
    decompose_by_cod(
      data_fig()$lt_initial,
      data_fig()$lt_final,
      data_fig()$cod_initial,
      data_fig()$cod_final
    )
  })

  # ----------------------------------------------------------------------------
  # RENDER datatables
  # Prepare data tables to add in the data tab
  
  table_captions <- reactive({
    generate_table_captions(
      input$mode,
      input$region1,
      input$region2,
      input$time_slider,
      input$sex,
      input$cod_change
      )
  })
  
  output$lt_initial  = DT::renderDataTable({
    data_fig()$lt_initial %>% 
      select(-region, -period, -sex) %>% 
      rename(
        `Age Interval` = x.int,
        `Age, (x)` = x
        ) %>% 
      format_datatable(
        caption = table_captions()[1]
      )
  })
  
  output$lt_final = DT::renderDataTable({
    data_fig()$lt_final %>% 
      select(-region, -period, -sex) %>% 
      rename(
        `Age Interval` = x.int,
        `Age (x)` = x) %>% 
      format_datatable(
        caption = table_captions()[2]
      )
  })
  
  output$cod_initial = DT::renderDataTable({
    data_fig()$cod_initial %>% 
      select(-region, -period, -sex) %>% 
      pivot_wider(
        names_from = cause_name,
        values_from = deaths) %>% 
      rename(`Age (x)` = x,) %>% 
      format_datatable(
        caption = table_captions()[3]
      )
  })
  
  output$cod_final = DT::renderDataTable({
    data_fig()$cod_final %>% 
      select(-region, -period, -sex) %>% 
      pivot_wider(
        names_from = cause_name,
        values_from = deaths) %>% 
      rename(`Age (x)` = x,) %>% 
      format_datatable(
        caption = table_captions()[4]
      )
  })
  
  output$decomposition_data <- DT::renderDataTable({
    data_decomp() %>% 
      select(-region, -period, -sex, -x.int) %>% 
      mutate(decomposition = round(decomposition, 6)) %>% 
      pivot_wider(
        names_from = cause_name,
        values_from = decomposition) %>% 
      rename(`Age (x)` = x,) %>% 
      format_datatable(
        caption = table_captions()[5]
      )
      
  })
  # ----------------------------------------------------------------------------
  # RENDER FIGURES

  # Figure 1 - The Map
  output$figure1 <- renderLeaflet({
    
    # We would like to zoom out if the region surface is large
    macro_region <- lemur::data_app_input$regions
    large_regions <- c(
      "ARGENTINA", 
      "ALGERIA", 
      "AUSTRALIA", 
      "BRAZIL", 
      "CANADA", 
      "CHILE", 
      "INDIA", 
      "JAPAN", 
      "MOROCCO", 
      "RUSSIA", 
      "SWEDEN", 
      "NORWAY", 
      "FINLAND", 
      "KAZAKHSTAN", 
      "US")
    
    loc <- input$region1
    if (input$region1 %in% large_regions) {
      zoom = 4
      
    } else if (input$region1 %in% macro_region) {
      zoom = 1
      loc <- "TUNISIA" 
      # Since we don't have the borders for the macro regions
      # select a location in the middle of the map and zoom out
      # just to display the map of the world
      
    } else {
      zoom = 5
      
    }
    
    suppressWarnings(
      plot_map(location = loc, zoom = zoom)
      )
    }
  )

  # Figure 2 - The change
  output$figure2 <- renderPlotly({
    
    # create figure caption
    fig2_caption <- generate_fig2_captions(
      input$mode,
      input$region1,
      input$region2,
      input$fig2_x,
      input$perc,
      input$cod_change,
      input$cod_target,
      data_fig()$lt_initial,
      data_fig()$lt_final
      )
      
    # create ggplot
    p2 <- plot_change(
      L1 = data_fig()$lt_final,
      L2 = data_fig()$lt_initial,
      age = input$fig2_x,
      perc = input$perc) +
      geom_point(size = 3) +
      labs(x = "", y = "") +
      theme(
        axis.text = element_text(size = 10)
      )

    # ggplot -> ggplotly
    p2 <- ggplotly(p2, tooltip = c("x", "y")) %>%
      plotly::layout(
        xaxis = list(title = fig2_caption),
        yaxis = list(title = "Age (years)")) %>%
      plotly::layout(
        xaxis = list(titlefont = list(size = 13), tickfont = list(size = 11)),
        yaxis = list(titlefont = list(size = 14), tickfont = list(size = 11))
        )

    p2

  })

  # Figure 3 - The COD Distribution
  output$figure3 <- renderPlotly({

    if (input$mode == "mode_cod") {
      p <- plot_cod(
        cod  = data_fig()$cod_final,
        perc = input$perc,
        type = "barplot")

    } else if (input$mode == "mode_cntr") {
      cod <- bind_rows(
        data_fig()$cod_initial,
        data_fig()$cod_final)

      p <- plot_cod(
        cod  = cod,
        perc = input$perc,
        type = "barplot") +
        facet_wrap("region")

    } else if (input$mode == "mode_sex") {
      cod <- bind_rows(
        data_fig()$cod_initial,
        data_fig()$cod_final)

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
    
    xlab <- generate_fig3_captions(input$perc)

    p <- ggplotly(p, tooltip = c("fill", "x")) %>%
      plotly::layout(
        xaxis = list(title = xlab)) %>%
      plotly::layout(
        xaxis = list(titlefont = list(size = 14), tickfont = list(size = 11)),
        yaxis = list(titlefont = list(size = 14), tickfont = list(size = 11)))

    p
  })

  # Figure 4 - The Decomposition
  output$figure4 <- renderPlotly({

    fig4_captions <- generate_fig4_captions(
      input$perc,
      input$fig4_dim
    )

    p4 <- plot_decompose(
      object = data_decomp(),
      perc   = input$perc,
      by     = input$fig4_dim
      )

    p4 <- ggplotly(p4, tooltip = fig4_captions$ttip) %>%
      plotly::layout(
        xaxis = list(title = fig4_captions$xlab),
        yaxis = list(title = fig4_captions$ylab)) %>%
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
  
  observeEvent(input$mode, {
    
    # If we move into region or sex comparison mode we set the risk reduction
    # slider to 0, since our main interest would be to see the differences 
    # as is before playing with hypothetical reductions of the cod's.   
    if (input$mode != "mode_cod") {
      updateSliderInput(
        session,
        inputId = "cod_change",
        value = 0
      )
    }
  })

  observeEvent(input$cod_target_none, {
    updatePrettyCheckboxGroup(
      session,
      inputId = "cod_target",
      selected = "none"
    )
  })
  
  # THE RESER EVENT
  observeEvent(input$reset, {
    updateRadioGroupButtons(session, 'mode', selected = "mode_cod")
    updateRadioGroupButtons(session, 'sex', selected = "both")
    updateSwitchInput(session, 'perc', value = FALSE)
    # updateSelectInput(session, 'region1', selected = "GLOBAL")
    # updateSelectInput(session, 'region2', selected = "EUROPE")
    updateSelectInput(session, 'fig2_x', selected = seq(0, 110, 10))
    updateSliderTextInput(session, 'time_slider', selected = 2019)
    updateSliderTextInput(session, 'age_change', selected = c(0, 110))
    updateSliderInput(session, 'cod_change', value = 0)
    updateSliderInput(session, 'sdg_1', value = 0)
    updateSliderInput(session, 'sdg_3', value = 0)
    updateSliderInput(session, 'sdg_4', value = 0)
    updateSliderInput(session, 'sdg_5', value = 0)
    updateSliderInput(session, 'sdg_6', value = 0)
    updateSliderInput(session, 'sdg_7', value = 0)
    updatePrettyCheckboxGroup(session, 'cod_target', selected = lemur::data_app_input$cause_name)
  })

}


# ----------------------------------------------------------------------------

#' Filter dataset using data.table methods
#' @keywords internal
dt_filter <- function(data, mode, region1, region2, gender, year) {

  region = period = sex <- NULL

  # we use data.table method to filter here because is faster
  # and we will do this all a lot
  dt <- as.data.table(data)
  dt <- dt[period == year]
  dt <- dt[region %in% c(region1, region2)]

  if (mode != "mode_sex") {
    dt <- dt[sex == gender]
  }

  return(as_tibble(dt))
}


#' @keywords internal
format_datatable <- function(data, caption){
  DT::datatable(
    data = format(
      x = as.data.frame(data),
      big.mark = ",",
      scientific = FALSE,
      digits = 2
    ),
    caption = caption,
    rownames= FALSE,
    # filter = 'top',
    options = list(
      # dom = 't',
      pageLength = 25,
      autoWidth = TRUE
    )
  )
}
