# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Thu Jun 10 22:12:29 2021
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
        level == "median")
    
  })
  
  # life tables data
  data_lt  <- reactive({
    MortalityCauses::data_gbd2019_lt %>% 
      dplyr::filter(
        region %in% c(input$region1, input$region2),
        sex == input$sex,
        level == "median")
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
    if (input$mode == 'cod_change') {
      prepare_data_mode1(
        data_cod(), 
        data_lt(), 
        input$region1, 
        input$region2, 
        input$cod_target,
        data_cod_change()
      )
      
    } else if (input$mode == 'cntr_compare') {
      prepare_data_mode2(
        data_cod(), 
        data_lt(), 
        input$region1, 
        input$region2,
        input$cod_target,
        data_cod_change()
      )
      
    } else if (input$mode == 'sex_compare') {
      prepare_data_mode3(
        input$region1, 
        input$cod_target,
        data_cod_change()
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
    plot_map()
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
    
    if (input$mode == "cntr_compare") {
      cod <- bind_rows(
        data_fig()$cod1, 
        data_fig()$cod2
        )
      
      plot_cod(
        cod = cod, 
        perc = input$perc,
        type = input$fig3_chart_type) + 
        facet_wrap("region")
      
    } else if (input$mode == 'cod_change') {
      plot_cod(
        cod = data_fig()$cod2, 
        perc = input$perc, 
        type = input$fig3_chart_type)
      
    } else if (input$mode == "sex_compare") {
      cod <- bind_rows(
        data_fig()$cod1, 
        data_fig()$cod2
        )
      
      plot_cod(
        cod = cod, 
        perc = input$perc, 
        type = input$fig3_chart_type) + 
        facet_wrap("sex")
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


#' Prepare data for risk changes
#' @keywords internal
prepare_data_mode1 <- function(cod, 
                               lt, 
                               region1, 
                               region2, 
                               cod_target, 
                               cod_change) {
  
  # Select cod and lt for 1 region
  # If no risk change is applied the tables before and after are the same
  # not change in LE no decomposition
  c1 <- dplyr::filter(cod, region == region1)
  c2 <- c1
  l1 <- dplyr::filter(lt, region == region1)
  l2 <- l1
  
  # IF there is a change applied we take the initial tables and
  # we modify them
  logic <- any(cod_change != 0) & !is.null(cod_target)
    if(logic) {
    c2 <- modify_cod_table(c1, cod_change)
    l2 <- modify_life_table(l1, c1, cod_change)
  }
  
  out <- list(cod1 = c1, cod2 = c2, lt1 = l1, lt2 = l2)
  return(out)
} 


#' Prepare data for country comparisons
#' @keywords internal
prepare_data_mode2 <- function(cod, 
                               lt, 
                               region1, 
                               region2, 
                               cod_target, 
                               cod_change) {
  
  # select cod and lt tables for 2 regions
  c1 <- dplyr::filter(cod, region == region1)
  c2 <- dplyr::filter(cod, region == region2)
  l1 <- dplyr::filter(lt, region == region1)
  l2 <- dplyr::filter(lt, region == region2)
  
  # IF we look at 2 regions and we change the risks 
  # we need to adjust the cod and lt tables for both regions
  logic <- any(cod_change != 0) & !is.null(cod_target)
  if(logic) {
    c1 <- modify_cod_table(c1, cod_change)
    c2 <- modify_cod_table(c2, cod_change)
    l1 <- modify_life_table(l1, c1, cod_change)
    l2 <- modify_life_table(l2, c2, cod_change)
  }
  
  c1 <- mutate(c1, region = factor(region, levels = c(region1, region2)))
  c2 <- mutate(c2, region = factor(region, levels = c(region1, region2)))
  
  out <- list(cod1 = c1, cod2 = c2, lt1 = l1, lt2 = l2)
  return(out)
}
  

#' Prepare data for sex comparisons
#' @keywords internal
prepare_data_mode3 <- function(region1, 
                               cod_target, 
                               cod_change){
  
  cod <- MortalityCauses::data_gbd2019_cod %>% 
    dplyr::filter(
      region == region1,
      level == "median")
  
  lt <- MortalityCauses::data_gbd2019_lt %>% 
    dplyr::filter(
      region == region1,
      level == "median")
  
  # select cod and lt tables for the 2 sexes
  c1 <- dplyr::filter(cod, sex == "male")
  c2 <- dplyr::filter(cod, sex == "female")
  l1 <- dplyr::filter(lt, sex == "male")
  l2 <- dplyr::filter(lt, sex == "female")
  
  # IF we look at 2 gender and we change the risks 
  # we need to adjust the cod and lt tables for both populations
  logic <- any(cod_change != 0) & !is.null(cod_target)
  if(logic) {
    c1 <- modify_cod_table(c1, cod_change)
    c2 <- modify_cod_table(c2, cod_change)
    l1 <- modify_life_table(l1, c1, cod_change)
    l2 <- modify_life_table(l2, c2, cod_change)
  }

  out <- list(cod1 = c1, cod2 = c2, lt1 = l1, lt2 = l2)
  return(out)
  
}


