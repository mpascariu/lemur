# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Thu Oct 28 00:07:23 2021
# --------------------------------------------------- #

#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#' @noRd
app_server <- function(input, output, session) {
  
  # INPUT DATA
  # cod data ---
  data_cod <- reactive({
    MortalityCauses::data_gbd2019_cod %>%
      dplyr::filter(
        region %in% c(input$region1, input$region2),
        sex == input$sex,
        period == input$time_slider)
  })
  
  # sdg data ---
  data_sdg <- reactive({
    MortalityCauses::data_gbd2019_sdg %>% 
      dplyr::filter(
        region %in% c(input$region1, input$region2),
        sex == input$sex,
        period == input$time_slider)
  })
  
  # life tables data
  data_lt  <- reactive({
    MortalityCauses::data_gbd2019_lt %>% 
      dplyr::filter(
        region %in% c(input$region1, input$region2),
        sex == input$sex,
        period == input$time_slider)
    }) 
  
  # Reduction matrix
  data_cod_change <- reactive({
    build_reduction_matrix(
      data = data_cod(), 
      select_cod = input$cod_target,
      select_x = input$age_change,
      cod_change = input$cod_change
    )
  })
  
  # Prepare data for figures depending on with mode is selected
  data_fig <- reactive({
    if (input$mode == 'mode_cod') {
      prepare_data_mode_cod(
        data_cod(), 
        data_lt(), 
        input$region1, 
        input$region2, 
        input$cod_target,
        data_cod_change()
      )
      
    } else if (input$mode == 'mode_cntr') {
      prepare_data_mode_cntr(
        data_cod(), 
        data_lt(), 
        input$region1, 
        input$region2,
        input$cod_target,
        data_cod_change()
      )
      
    } else if (input$mode == 'mode_sex') {
      prepare_data_mode_sex(
        input$region1, 
        input$cod_target,
        data_cod_change(),
        year = input$time_slider
      )
      
    } else if (input$mode == 'mode_sdg') {
      prepare_data_mode_cod(
        cod        = data_sdg(), 
        lt         = data_lt(), 
        region1    = input$region1, 
        region2    = input$region2, 
        cod_target = input$cod_target,
        cod_change = data_cod_change()
      )
    }
    
  })
  
  # Decompose the difference in life expectancy at birth
  decomp <- reactive({
    with(data_fig(), decompose_by_cod(lt1, lt2, cod1, cod2))
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
  output$figure2 <- renderPlot({
    plot_change(
      L1 = data_fig()$lt2,
      L2 = data_fig()$lt1,
      age = input$fig2_x,
      perc = input$perc
    )
  })
  
  # Figure 3 - The COD Distribution
  output$figure3 <- renderPlot({
    
    if (input$mode == "mode_cntr") {
      cod <- bind_rows(
        data_fig()$cod1, 
        data_fig()$cod2
        )
      
      plot_cod(
        cod = cod, 
        perc = input$perc,
        type = input$fig3_chart_type) + 
        facet_wrap("region")
      
    } else if (input$mode == "mode_cod") {
      plot_cod(
        cod = data_fig()$cod2, 
        perc = input$perc, 
        type = input$fig3_chart_type)
      
    } else if (input$mode == "mode_sex") {
      cod <- bind_rows(
        data_fig()$cod1, 
        data_fig()$cod2
        )
      
      plot_cod(
        cod = cod, 
        perc = input$perc, 
        type = input$fig3_chart_type) + 
        facet_wrap("sex")
      
    } else if (input$mode == "mode_sdg") {
      plot_cod(
        cod = data_fig()$cod2, 
        perc = input$perc, 
        type = input$fig3_chart_type)
    }
    
  })
  
  # Figure 4 - The Decomposition
  output$figure4 <- renderPlot(
    plot_decompose(
      decomp(), 
      perc = input$perc,
      by = input$fig4_dim)
  )
  
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


