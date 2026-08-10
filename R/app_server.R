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

  # Hide the loading overlay once the UI is fully rendered
  observe({
    if (ui_state$ready) {
      session$sendCustomMessage("hideLoading", list())
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

  # The year selector. Deliberately NOT debounced: the single debounce on
  # data_fig() below gates the whole figure/table cascade, so dragging this
  # slider only invalidates the cheap cache reads in data_cod/data_sdg/data_lt,
  # not the expensive computation downstream.
  time_slider <- reactive(input$time_slider)

  # Pre-load the static datasets as data.tables once at startup, so that
  # dt_filter_local() filters without re-converting the full tables on every
  # input change. (Not needed in server mode where the data lives in Postgres.)
  #
  # The data lives in inst/extdata/*_dt.rds -- lean copies of the public .rda
  # datasets (see data-raw/build_fast_data.R): pre-factorized data.tables with
  # gzip compression. They deserialize ~18x faster than the bzip2 .rda files
  # (0.5s vs 8.6s for COD) and skip the as.data.table() conversion entirely.
  if (!isTRUE(getShinyOption("serverMode"))) {
    read_fast <- function(f) readRDS(system.file("extdata", f, package = "lemur"))
    session$userData$dt <- list(
      cod = read_fast("cod_dt.rds"),
      lt  = read_fast("lt_dt.rds")
      # sdg is loaded lazily -- only when the user enters an SDG mode
    )
  }

# ------------------------------------------------------------------
# Helper to fetch data via SQL or local, avoiding eval/call
# ------------------------------------------------------------------
fetch_data <- function(data_sql_name) {
  if (serverMode()) {
    dt_filter_sql(
      data    = data_sql_name,
      mode    = input$mode,
      region1 = input$region1,
      region2 = input$region2,
      gender  = input$sex,
      year    = time_slider(),
      db_pool = db_pool()
    )

  } else {
    dt_filter_local(
      data    = session$userData$dt[[data_sql_name]],
      mode    = input$mode,
      region1 = input$region1,
      region2 = input$region2,
      gender  = input$sex,
      year    = time_slider(),
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
    fetch_data("cod") %>%
      mutate(cause_name = factor(cause_name, levels = data_app_input$cause_name)) %>%
      arrange(region, sex, x, cause_name) %>%
      as_tibble()
  }
}) |> bindCache(
  input$mode, input$region1, input$region2, input$sex,
  time_slider(), serverMode()
)
#
# ------------------------------------------------------------------
# 2) sdg data
# ------------------------------------------------------------------
data_sdg <- reactive({
  req(ui_state$ready)

  if (input$mode %in% c("mode_sdg", "mode_sdg2")) {
    # SDG data is loaded lazily on first access: the sdg table (3.1M rows) is
    # only needed in the two SDG modes, so we skip its ~0.6s load for every
    # session that never uses it.
    if (is.null(session$userData$dt[["sdg"]])) {
      session$userData$dt[["sdg"]] <- readRDS(
        system.file("extdata", "sdg_dt.rds", package = "lemur")
      )
    }
    fetch_data("sdg") %>%
      mutate(cause_name = factor(cause_name, levels = data_app_input$cause_name_sdg)) %>%
      arrange(region, sex, x, cause_name) %>%
      as_tibble()
  }
}) |> bindCache(
  input$mode, input$region1, input$region2, input$sex,
  time_slider(), serverMode()
)

# ------------------------------------------------------------------
# 3) life tables data
# ------------------------------------------------------------------
data_lt <- reactive({
  req(ui_state$ready)
  
  lt <- fetch_data("lt")
  
  if (serverMode()) {
    lt <- lt %>% rename(x.int = x_int, Lx = llx, Tx = ttx)
  }
  
  # critical fix: return a proper tibble copy with consistent ordering 
  as_tibble(lt) %>% arrange(region, sex, x)
}) |> bindCache(
  input$mode, input$region1, input$region2, input$sex,
  time_slider(), serverMode()
)

  # Reduction matrix -----------------------------
  data_cod_change <- reactive({
    req(ui_state$ready)
    
    if (input$mode == "mode_sdg") {

      d_sdg <- data_sdg()
      M <- build_reduction_matrix(
        data       = d_sdg,
        select_cod = as.character(unique(d_sdg$cause_name)),
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

      d_sdg <- data_sdg()
      M <- build_reduction_matrix(
        data       = d_sdg,
        select_cod = as.character(unique(d_sdg$cause_name)),
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
    # NOTE: deliberately NOT debounced. data_fig() is the single debounce point
    # of the app. Debouncing this matrix too re-opens a window in which data_fig
    # sees fresh data with a stale (wrong-shaped) reduction matrix - the cause of
    # the "non-conformable arrays" crash when switching modes.
  })

  # Prepare data for figures depending on which mode is selected.
  #
  # data_fig is THE single debounce point of the app. It bundles the prepared
  # figure data together with the reduction matrix and every scalar input the
  # figures/tables/captions read, and the whole bundle is debounced. That
  # guarantees that after any input change the UI recomputes exactly once from
  # one consistent snapshot: no intermediate state where a figure renders with
  # fresh data but a stale reduction matrix (crashed with "non-conformable
  # arrays" on mode switches) and no double re-render from two debounce timers
  # firing in series.
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

    out <- switch(input$mode,
      mode_cod  = process_data(x = data_cod(), mode = "cod"),
      mode_cntr = process_data(x = data_cod(), mode = "cntr"),
      mode_sex  = process_data(x = data_cod(), mode = "sex"),
      mode_sdg  = process_data(x = data_sdg(), mode = "cod"),
      mode_sdg2 = process_data(x = data_sdg(), mode = "cod")
    )

    list(
      data       = out,
      reduction  = data_cod_change(),
      mode       = input$mode,
      region1    = input$region1,
      region2    = input$region2,
      sex        = input$sex,
      year       = input$time_slider,
      fig2_x     = input$fig2_x,
      perc       = input$perc,
      cod_change = input$cod_change,
      cod_target = input$cod_target,
      fig4_dim   = input$fig4_dim
    )
  }) |> shiny::debounce(getShinyOption("lemur.debounce", default = 250))
  
  # Decompose the difference in life expectancy at birth
  data_decomp <- reactive({
    req(ui_state$ready)

    # Read the whole (debounced) figure snapshot once, so this reactive never
    # observes an inconsistent mix of fresh data and a stale reduction matrix.
    df <- data_fig()

    # When no risk change is applied the initial and final life tables are
    # identical, so the stepwise-replacement decomposition would be all zeros.
    # Skip that computation; figure 4 and the decomposition table show a
    # placeholder instead (see their renderers below).
    if (!any(df$reduction != 0)) {
      return(NULL)
    }

    decompose_by_cod(
      df$data$lt_initial,
      df$data$lt_final,
      df$data$cod_initial,
      df$data$cod_final
    )
  })
  
  #----------------------------------------------------------------------------
  
  # Define table and figure captions
  table_captions <- reactive({
    req(ui_state$ready)

    # Take every argument from the debounced data_fig() snapshot so the table
    # captions always describe exactly the data being displayed.
    df <- data_fig()

    generate_table_captions(
      df$mode,
      df$region1,
      df$region2,
      df$year,
      df$sex,
      df$cod_change
    )
  })
  
  figure_captions <- reactive({
    req(ui_state$ready)

    # All arguments come from the debounced data_fig() snapshot, so captions
    # always describe exactly the data the figures show.
    df <- data_fig()

    generate_figure_captions(
      df$mode,
      df$region1,
      df$region2,
      df$fig2_x,
      df$perc,
      df$cod_change,
      df$cod_target,
      df$data$lt_initial,
      df$data$lt_final,
      df$fig4_dim
    )
  })
  
  # ----------------------------------------------------------------------------
  # RENDER datatables
  # Prepare data tables to add in the data tab
  
  
  output$lt_initial  = DT::renderDataTable({
    req(data_fig())
    
    data_fig()$data$lt_initial %>%
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
    
    data_fig()$data$lt_final %>%
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
    
    data_fig()$data$cod_initial %>%
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
    
    data_fig()$data$cod_final %>%
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
    req(data_fig())

    # No risk change applied: nothing to decompose, show a note instead of an
    # all-zero table.
    if (is.null(data_decomp())) {
      return(
        DT::datatable(
          data.frame(Note = "No risk change applied - adjust a risk to see the decomposition."),
          caption = table_captions()[5],
          rownames = FALSE,
          options = list(pageLength = 1)
        )
      )
    }

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
    df <- data_fig()
    req(df$reduction)

    df$reduction %>%
      as_tibble() %>%
      mutate(`Age Group` = df$data$lt_initial$x.int, .before = 1) %>%
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
    df <- data_fig()
    req(df$data)

    cap <- figure_captions()

    plotly_change(
      L1   = df$data$lt_final,
      L2   = df$data$lt_initial,
      age  = df$fig2_x,
      perc = df$perc,
      xlab = cap$fig2$xlab,
      ylab = cap$fig2$ylab
    )
  })
  
  # Figure 3 - The COD Distribution
  output$figure3 <- renderPlotly({
    # Stop execution if no dataset is selected
    req(ui_state$ready)
    df <- data_fig()
    req(df$data)

    cap <- figure_captions()

    # The comparison modes (cntr/sex) plot initial vs final COD side by side,
    # everything else plots the final COD distribution only.
    cod <- if (df$mode %in% c("mode_cntr", "mode_sex")) {
      bind_rows(df$data$cod_initial, df$data$cod_final)
    } else {
      df$data$cod_final
    }

    plotly_cod(
      cod  = cod,
      perc = df$perc,
      xlab = cap$fig3,
      mode = df$mode
    )
  })
  
  # Figure 4 - The Decomposition
  output$figure4 <- renderPlotly({
    # Stop execution if no dataset is selected
    req(ui_state$ready)
    df <- data_fig()
    req(df$data)

    # No risk change applied: the decomposition would be all zeros, so show a
    # message instead of an empty/meaningless chart.
    dec <- data_decomp()
    if (is.null(dec)) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "markers") %>%
          plotly::layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(
                text = "No risk change applied.<br>Adjust a risk to see the decomposition.",
                showarrow = FALSE,
                x = 0.5, y = 0.5,
                xref = "paper", yref = "paper",
                font = list(size = 14)
              )
            )
          )
      )
    }

    cap <- figure_captions()

    plotly_decompose(
      object = dec,
      perc   = df$perc,
      by     = df$fig4_dim,
      xlab   = cap$fig4$xlab,
      ylab   = cap$fig4$ylab,
      ttip   = cap$fig4$ttip
    )
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

  # ----------------------------------------------------------------------------
  # OUTPUT VISIBILITY
  # Don't spend time rendering outputs that sit in hidden tabs/panels (e.g. the
  # data tables under the "Data" tab while the dashboard is being viewed).
  invisible(lapply(
    c("lt_initial", "lt_final", "cod_initial", "cod_final",
      "decomposition_data", "reduction_matrix",
      "figure1", "figure2", "figure3", "figure4"),
    function(id) shiny::outputOptions(output, id, suspendWhenHidden = TRUE)
  ))

}




