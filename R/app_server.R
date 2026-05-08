# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-08 18:18:33
# --------------------------------------------

#' The application server-side
#'
#' @param input,output,session Internal parameters for shiny.
#' @keywords internal
#' @export
app_server <- function(input, output, session) {
  
  # Initialize reactive values
  ui_state <- reactiveValues(
    ready = FALSE
  )
  
  # Check when UI is fully rendered
  observe({
    # Check multiple critical inputs
    critical_inputs_ready <- all(
      !is.null(input$mode),
      !is.null(input$region1),
      !is.null(input$region2),
      !is.null(input$sex),
      !is.null(input$time_slider),
      !is.null(input$age_change),
      !is.null(input$cod_change),
      !is.null(input$cod_target),
      !is.null(input$cod_target_all),
      !is.null(input$cod_target_none),
      !is.null(input$fig2_x),
      !is.null(input$fig4_dim),
      !is.null(input$perc)
    )
    
    if (critical_inputs_ready && !ui_state$ready) {
      ui_state$ready <- TRUE
    }
  })
  
  # INPUT DATA selection
  # The source of data can be the local datasets saved in the package or the 
  # datasets saved in a postgresSQL and hosted externally on a server. The 
  # code below may apear a bit complex but it's main purpose is to select the
  # indicared source and to query the data according to input in the dashboard.
  # If serverMode = TRUE, we read the postgresSQL data otherwise the local data.
  
  serverMode <- reactive(getShinyOption("serverMode"))
  
  # Initialize pool once at app startup
  session$userData$pool <- create_db_pool(run_db = getShinyOption("serverMode"))
  
  # Clean up on app stop
  onStop(function() {
    if (!is.null(session$userData$pool)) {
      pool::poolClose(session$userData$pool)
    }
  })
  
  # Simple reactive wrapper for use in data reactives
  db_pool <- reactive({
    session$userData$pool
  })
  
# ------------------------------------------------------------------
# Helper to fetch data via SQL or local, avoiding eval/call
# ------------------------------------------------------------------
fetch_data <- function(data_local, data_sql_name) {
  if (serverMode()) {
    dt_filter_sql(
      data    = data_sql_name,
      mode    = input$mode,
      region1 = input$region1,
      region2 = input$region2,
      gender  = input$sex,
      year    = input$time_slider,
      db_pool = db_pool()
    )
    
  } else {
    dt_filter_local(
      data    = data_local,
      mode    = input$mode,
      region1 = input$region1,
      region2 = input$region2,
      gender  = input$sex,
      year    = input$time_slider,
      db_pool = db_pool()
    )
  }
}

# ------------------------------------------------------------------
# 1) cod data
# ------------------------------------------------------------------
data_cod <- reactive({
  req(ui_state$ready)
  
  if (input$mode %in% c("mode_cod", "mode_sex", "mode_cntr")) {
    fetch_data(data_gbd2021_cod, "cod") %>%
      mutate(cause_name = factor(cause_name, levels = data_app_input$cause_name)) %>%
      arrange(region, sex, x, cause_name) %>%
      as_tibble()
  }
}) #|> 
   # bindCache(input$mode, input$region1, input$region2, input$sex, input$time_slider, serverMode())
#
# ------------------------------------------------------------------
# 2) sdg data
# ------------------------------------------------------------------
data_sdg <- reactive({
  req(ui_state$ready)
  
  if (input$mode %in% c("mode_sdg", "mode_sdg2")) {
    fetch_data(data_gbd2021_sdg, "sdg") %>%
      mutate(cause_name = factor(cause_name, levels = data_app_input$cause_name_sdg)) %>%
      arrange(region, sex, x, cause_name) %>%
      as_tibble()
  }
}) #|> 
   # bindCache(input$mode, input$region1, input$region2, input$sex, input$time_slider, serverMode())

# ------------------------------------------------------------------
# 3) life tables data
# ------------------------------------------------------------------
data_lt <- reactive({
  req(ui_state$ready)
  
  lt <- fetch_data(data_gbd2021_lt, "lt")
  
  if (serverMode()) {
    lt <- lt %>% rename(x.int = x_int, Lx = llx, Tx = ttx)
  }
  
  # critical fix: return a proper tibble copy with consistent ordering 
  as_tibble(lt) %>% arrange(region, sex, x)
}) #|> 
   # bindCache(input$mode, input$region1, input$region2, input$sex, input$time_slider, serverMode())

  # Reduction matrix -----------------------------
  data_cod_change <- reactive({
    req(ui_state$ready)
    
    if (input$mode == "mode_sdg") {
      
      M <- build_reduction_matrix(
        data       = data_sdg(),
        select_cod = as.character(unique(data_sdg()$cause_name)),
        select_x   = 0:110,
        cod_change = 0
      )
      
      S1  = c("0", "1", "2")   # Under-five mortality 
      S2a = "Maternal disorders"
      S2b = "Neonatal disorders"
      S3 = c("HIV/ AIDS / STD",
             "Tuberculosis",
             "Malaria",
             "Neglected Tropical Diseases (excl. Malaria)")
      S4 = c("Cardiovascular Diseases",
             "Neoplasms",
             "Diabetes mellitus",
             "Chronic Respiratory diseases")
      S5 = "Self-harm"
      S6 = "Transport Injuries"
      S7 = "Exposure to forces of nature"
      
      if (input$sex != 'male') {
        # For now males are not exposed to maternal disorders :)
        M[  , S2a] <- input$sdg_2a
      }
      M[  , S2b] <- input$sdg_2b
      M[  , S3] <- input$sdg_3
      M[  , S4] <- input$sdg_4
      M[  , S5] <- input$sdg_5
      M[  , S6] <- input$sdg_6
      M[  , S7] <- input$sdg_7
      
      # when under 5 mortality is reduced across all COD we have to deal with 
      # interactions, or successive reduction inputs. E.g. One may reduce 
      # neonatal mortality (50%) and under-five mortality (10%) resulting a 55%
      # total reduction. This is what we try to do in the next 5 lines.
      
      if (input$sdg_1 != 0) {
        if (sum(M[S1, ]) != 0) {
          M[S1,   ] <- ((1 + input$sdg_1/100) * ((M[S1, ] + 100)/100) - 1) * 100
          
        } else  {
          M[S1,   ] <- input$sdg_1 
        }
      }
      
    } else if (input$mode == 'mode_sdg2') {
      
      M <- build_reduction_matrix(
        data       = data_sdg(),
        select_cod = as.character(unique(data_sdg()$cause_name)),
        select_x   = 0:110,
        cod_change = 0
      )
      
      M[  , "Cardiovascular Diseases"]                     <- input$sdg2_1  
      M[  , "Chronic Respiratory diseases"]                <- input$sdg2_2  
      M[  , "Diabetes mellitus"]                           <- input$sdg2_3  
      M[  , "Enteric Infections"]                          <- input$sdg2_4  
      M[  , "Exposure to forces of nature"]                <- input$sdg2_5  
      M[  , "HIV/ AIDS / STD"]                             <- input$sdg2_6  
      M[  , "Injuries (excl. Poisonings)"]                 <- input$sdg2_7  
      M[  , "Interpersonal Violence"]                      <- input$sdg2_8  
      M[  , "Kidney disease (excl. Diabetes)"]             <- input$sdg2_9  
      M[  , "Malaria"]                                     <- input$sdg2_10  
      if (input$sex != 'male') M[  , "Maternal disorders"] <- input$sdg2_11
      M[  , "Neglected Tropical Diseases (excl. Malaria)"] <- input$sdg2_12  
      M[  , "Neonatal disorders"]                          <- input$sdg2_13  
      M[  , "Neoplasms"]                                   <- input$sdg2_14  
      M[  , "Other Communicable"]                          <- input$sdg2_15  
      M[  , "Other Non-Communicable"]                      <- input$sdg2_16  
      M[  , "Poisonings"]                                  <- input$sdg2_17  
      M[  , "Respiratory Infections (excl. Tuberculosis)"] <- input$sdg2_18  
      M[  , "Self-harm"]                                   <- input$sdg2_19  
      M[  , "Transport Injuries"]                          <- input$sdg2_20  
      M[  , "Tuberculosis"]                                <- input$sdg2_21  
      
      
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
    req(ui_state$ready)
    
    process_data <- function(x, mode) {
      prepare_data(
        cod        = x,
        lt         = data_lt(),
        region1    = input$region1,
        region2    = input$region2,
        sex        = input$sex,
        cod_change = data_cod_change(),
        mode       = mode
      )
    }
    
    switch(input$mode,
           mode_cod  = process_data(x = data_cod(), mode = "cod"),
           mode_cntr = process_data(x = data_cod(), mode = "cntr"),
           mode_sex  = process_data(x = data_cod(), mode = "sex"),
           mode_sdg  = process_data(x = data_sdg(), mode = "cod"),
           mode_sdg2 = process_data(x = data_sdg(), mode = "cod")
    )
  })
  
  # Decompose the difference in life expectancy at birth
  data_decomp <- reactive({
    req(ui_state$ready)
    
    decompose_by_cod(
      data_fig()$lt_initial,
      data_fig()$lt_final,
      data_fig()$cod_initial,
      data_fig()$cod_final
    )
  })
  
  #----------------------------------------------------------------------------
  
  # Define table and figure captions
  table_captions <- reactive({
    req(ui_state$ready)
    
    generate_table_captions(
      input$mode,
      input$region1,
      input$region2,
      input$time_slider,
      input$sex,
      input$cod_change
    )
  })
  
  figure_captions <- reactive({
    req(ui_state$ready)
    
    generate_figure_captions(
      input$mode,
      input$region1,
      input$region2,
      input$fig2_x,
      input$perc,
      input$cod_change,
      input$cod_target,
      data_fig()$lt_initial,
      data_fig()$lt_final,
      input$fig4_dim
    )
  })
  
  # ----------------------------------------------------------------------------
  # RENDER datatables
  # Prepare data tables to add in the data tab
  
  
  output$lt_initial  = DT::renderDataTable({
    req(data_fig())
    
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
    req(data_fig())
    
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
    req(data_fig())
    
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
    req(data_fig())
    
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
    req(data_decomp())
    
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
  
  output$reduction_matrix <- DT::renderDataTable({
    req(data_cod_change())
    
    data_cod_change() %>% 
      as_tibble() %>%
      mutate(`Age Group` = data_fig()$lt_initial$x.int, .before = 1) %>% 
      format_datatable(
        caption = table_captions()[6]
      )
  })
  # ----------------------------------------------------------------------------
  # RENDER FIGURES

  # Figure 1 - The Map
  output$figure1 <- renderLeaflet({
    req(ui_state$ready)
    
    # We would like to zoom out if the region surface is large
    macro_region <- data_app_input$regions
    large_regions <- c(
      "Algeria", 
      "Australia", 
      "Canada", 
      "Chile", 
      "India", 
      "Japan", 
      "Morocco", 
      "Sweden", 
      "Norway", 
      "Finland", 
      "Kazakhstan")
    
    larger_regions <- c(
      "Argentina", 
      "Brazil", 
      "China (People's Republic of)", 
      "Russian Federation", 
      "United States of America")
    
    loc <- input$region1
    if (input$region1 %in% large_regions) {
      zoom = 4
      
    } else if (input$region1 %in% larger_regions) {
      zoom = 3
      
    } else if (input$region1 %in% macro_region) {
      zoom = 2
      loc <- "Malta" 
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
    # Stop execution if no dataset is selected
    req(ui_state$ready)
    req(data_fig())

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
        xaxis = list(title = figure_captions()$fig2$xlab),
        yaxis = list(title = figure_captions()$fig2$ylab)) %>%
      plotly::layout(
        xaxis = list(titlefont = list(size = 13), tickfont = list(size = 11)),
        yaxis = list(titlefont = list(size = 14), tickfont = list(size = 11))
      )
    
    p2
    
  })
  
  # Figure 3 - The COD Distribution
  output$figure3 <- renderPlotly({
    # Stop execution if no dataset is selected
    req(ui_state$ready)
    req(data_fig())
    
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
      
    } else if (input$mode %in% c("mode_sdg", "mode_sdg2")) {
      p <- plot_cod(
        cod  = data_fig()$cod_final,
        perc = input$perc,
        type = "barplot")
    }
    
    p <- p +
      labs(x = "", y = "") +
      scale_y_discrete(limits = rev) + 
      theme(
        axis.text = element_text(size = 7)
      )
    
    p3 <- ggplotly(p, tooltip = c("fill", "x")) %>%
      plotly::layout(
        xaxis = list(title = figure_captions()$fig3)) %>%
      plotly::layout(
        xaxis = list(titlefont = list(size = 14), tickfont = list(size = 11)),
        yaxis = list(titlefont = list(size = 14), tickfont = list(size = 11)))
    
    p3
  })
  
  # Figure 4 - The Decomposition
  output$figure4 <- renderPlotly({
    # Stop execution if no dataset is selected
    req(ui_state$ready)
    req(data_decomp())
    
    p4 <- plot_decompose(
      object = data_decomp(),
      perc   = input$perc,
      by     = input$fig4_dim
    )
    
    p4 <- ggplotly(p4, tooltip = figure_captions()$fig4$ttip) %>%
      plotly::layout(
        xaxis = list(title = figure_captions()$fig4$xlab),
        yaxis = list(title = figure_captions()$fig4$ylab)
        ) %>%
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
  
  observeEvent(input$region2, {
    if (input$region1 == input$region2) {
      showNotification(
        ui = "Select two distinct regions to allow for comparisons!",
        duration = 10,
        type = "error"
      )
    }
  })
  
  # THE RESET EVENT
  observeEvent(input$reset, {
    reset_inputs(session)
  })
  
}




