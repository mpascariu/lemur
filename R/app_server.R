# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed Feb 23 15:29:14 2022
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
  # Prepare data tables to and in the data tab
  
  # First compose captions for our tables:
  # We have 2 lifetables, 2 COD distribution tables and 1 decompposition table
  # Depending on what selection is performed on the dashboard we would need
  # to adjust the information in the caption of each table so that we 
  # keep the app intuitive.
  # 
  table_captions <- reactive({
    lt_initial = "NO CAPTION FOR THIS TABLE GIVEN THE CURRENT SELECTION. SUGGEST ONE, PLEASE!"
    lt_final = cod_initial = cod_final  = decomposition <- lt_initial

      part1 <- paste0(
        ifelse(input$sex == "both", "total ", input$sex), 
        " population, ", input$region1, " (", input$time_slider, ")"
        )
      part1.2 <- paste0(
        ifelse(input$sex == "both", "total ", input$sex), 
        " population, ", input$region2, " (", input$time_slider, ")"
        )

      part2 <- paste0(
        "The values in the table are not affected by changes ",
        "in mortality risks as applied in the dashboard."
        )
      
      part3 <- ifelse(
        input$cod_change != 0, 
        paste0(
          "The values are resulted from a change of ", 
          input$cod_change, "% applied to one or more risks factors ",
          "as specified in the dashboard."), 
        "")
      
      part4 <- paste0(
        "The values are resulted from various changes to the ", 
        "SDG accomplishment levels applied to one or more risks factors ",
        "as specified in the dashboard.") 
      
      
    if (input$mode %in% c("mode_cod", "mode_sdg")) {
      lt_initial <- paste0(
        "TABLE 1 -- Life table for ", part1, ". ", part2,
        " TABLE 2 below includes such alterations."
        )
      
      lt_final <- paste0(
        "TABLE 2 -- Hypothetical life table for ", part1, ". ", part3
        )
      cod_initial <- paste0(
        "TABLE 3 -- Distribution of deaths by cause for ", part1, ". ", part2,
        " TABLE 4 below includes such alterations."
        )
      cod_final <- paste0(
        "TABLE 4 -- Hypothetical distribution of deaths by cause for ", 
        part1, ". ", part3
        )
      decomposition <- paste0(
        "TABLE 5 -- Differences in life expectancy at birth attributable to ",
        "each cause of death for ", part1, ". ", part3
        )
    }
      
    if (input$mode == "mode_sdg") {

      lt_final <- paste0(
        "TABLE 2 -- Hypothetical life table for ", part1, ". ", part4
      )
      cod_final <- paste0(
        "TABLE 4 -- Hypothetical distribution of deaths by cause for ", 
        part1, ". ", part4
      )
      decomposition <- paste0(
        "TABLE 5 -- Differences in life expectancy at birth attributable to ",
        "each cause of death for ", part1, ". ", part4
      )
    }
      
    if (input$mode == "mode_cntr") {
      lt_initial <- paste0(
        "TABLE 1 -- Life table for ", part1, ". ", part3
      )
      lt_final <- paste0(
        "TABLE 2 -- Life table for ", part1.2, ". ", part3
      )
      cod_initial <- paste0(
        "TABLE 3 -- Distribution of deaths by cause for ", part1, ". ", part3
      )
      cod_final <- paste0(
        "TABLE 4 -- Distribution of deaths by cause for ", part1.2, ". ", part3
      )
      decomposition <- paste0(
        "TABLE 5 -- Differences in life expectancy at birth attributable to ",
        "each cause of death between ", input$region1, " and ", input$region2,
        " in ", input$time_slider, ". ",
        ifelse(input$cod_change != 0, paste0("In addition ", tolower(part3)), "")
      )
    }
    
    if (input$mode == "mode_sex") {
      lt_initial <- paste0(
        "TABLE 1 -- Life table for male population, ", 
        input$region1, " (", input$time_slider, "). ", part3
      )
      lt_final <- paste0(
        "TABLE 2 -- Life table for female population, ", 
        input$region1, " (", input$time_slider, "). ", part3
      )
      cod_initial <- paste0(
        "TABLE 3 -- Distribution of deaths by cause for male population, ",
        input$region1, " (", input$time_slider, "). ", part3
      )
      cod_final <- paste0(
        "TABLE 4 -- Distribution of deaths by cause for female population, ", 
        input$region1, " (", input$time_slider, "). ", part3
      )
      decomposition <- paste0(
        "TABLE 5 -- Sex-gap in life expectancy at birth attributable to ",
        "each cause of death, ", 
        input$region1, " (", input$time_slider, "). ",
        ifelse(input$cod_change != 0, paste0("In addition ", tolower(part3)), "")
      )
    }
      
    captions <- c(lt_initial, lt_final, cod_initial, cod_final, decomposition)
    captions
  })
  
  output$lt_initial  = DT::renderDataTable({
    data_fig()$lt_initial %>% 
      select(-region, -period, -sex) %>% 
      rename(
        `Age Interval` = x.int,
        `Age (x)` = x) %>% 
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
      pivot_wider(names_from = cause_name,
                  values_from = deaths) %>% 
      rename(`Age (x)` = x,) %>% 
      format_datatable(
        caption = table_captions()[3]
      )
  })
  
  output$cod_final = DT::renderDataTable({
    data_fig()$cod_final %>% 
      select(-region, -period, -sex) %>% 
      pivot_wider(names_from = cause_name,
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
      pivot_wider(names_from = cause_name,
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
    
    # Build a dynamic x-axis title to help with interpretability
    # Part 1 - absolute or relative values?
    x_min      <- min(as.numeric(input$fig2_x))
    x_min_text <- if (x_min == 0) " at birth" else paste(" at age", x_min)
    l0         <- data_fig()$lt_initial
    l1         <- data_fig()$lt_final
    
    
    suffix <- if (input$cod_change == 0) "in life expectancy" else ""
    xlab_part1 <- ifelse(
      input$perc, 
      paste0("Relative difference ", suffix),
      paste0("Difference ", suffix)
    )
    
    # Part 2 - increase, decrease how much, where? 
    xlab_part2 <- if(all(!is.null(input$cod_target)) & input$cod_change != 0) {
      paste0(
        " following a ", abs(input$cod_change), "%",
        ifelse(sign(input$cod_change) == -1, " reduction", " increase"),
        " in ",
        ifelse(length(input$cod_target) == 1,
               paste(input$cod_target, "related deaths"),
                     "multiple causes of death")
        )
      
    } else {
      ""
    }
    
    # Part 3 - Life expectancy before and after
    if (input$mode %in% c("mode_cod", "mode_sdg")) {
      prefix1 <- "Before: "
      prefix2 <- "After the changes: "
    }
    if (input$mode == "mode_sex") {
      prefix1 <- "Males: "
      prefix2 <- "Females: "
    }
    if (input$mode == "mode_cntr") {
      prefix1 <- paste0(input$region1, ": ")
      prefix2 <- paste0(input$region2, ": ")
    }
    
    xlab_part3 <- paste0(
      "\n[",
      prefix1, 
      round(l0$ex[l0$x == x_min], 2), 
      " vs. ", 
      prefix2, 
      round(l1$ex[l1$x == x_min], 2),
      " years", x_min_text, "]"
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
        xaxis = list(title = paste0(xlab_part1, xlab_part2, xlab_part3)),
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
    
    xlab <- if (input$perc) {
      "Proportion of the Total No. of Deaths\n[%]"
    } else {
      "Number of Deaths\n"
    }
    
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
