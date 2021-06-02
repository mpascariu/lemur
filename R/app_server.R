# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed Jun 02 21:06:06 2021
# --------------------------------------------------- #

#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#' @noRd
app_server <- function(input, output, session) {
  
  # The reactive UI has to live in the server... to be reactive
  output$ui_dashboard <- renderUI({
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
  })
  
  outputOptions(output, "ui_dashboard", suspendWhenHidden = FALSE)
  
  # ----------------------------------------------------------------------------
  # INPUT DATA
  
  

  
  # cod data ---
  data_cod <- reactive({
    MortalityCauses::data_gbd2019_cod %>% 
      dplyr::filter(
        region %in% c(input$region1, input$region2),
        sex == input$sex,
        level == "median")
    
  })
  
  data_cod1 <- reactive({
    data_cod() %>% 
      dplyr::filter(
        region == input$region1)
  })
  
  # Reduction matrix
  data_cod_change <- reactive({
    build_reduction_matrix(
      data = data_cod1(), 
      select_cod = input$cod_target,
      select_x = input$age_change,
      cod_change = input$cod_change
    )
  })
  
  data_cod2 <- reactive({
    if (input$mode == 'cntr_compare') {
      cod <- data_cod() %>% 
        dplyr::filter(
          region == input$region2)
    }
    
    if (input$mode == 'cod_change') {
      cod <- modify_cod_table(
        cod = data_cod1(), 
        cod_change = data_cod_change())
    }
    
    cod
  })
  

  
  # life tables data
  data_lt  <- reactive({
    MortalityCauses::data_gbd2019_lt %>% 
      dplyr::filter(
        region %in% c(input$region1, input$region2),
        sex == input$sex,
        level == "median")
    }) 
  
  data_lt1 <- reactive({
    data_lt() %>% 
      dplyr::filter(
        region == input$region1)
  })
  
  data_lt2 <- reactive({
    
    if (input$mode == 'cntr_compare') {
      lt <- data_lt() %>% 
        dplyr::filter(
          region == input$region2)
      
    } 
    
    if (input$mode == 'cod_change') {
      lt <- modify_life_table(
        lt = data_lt1(), 
        cod = data_cod1(), 
        cod_change = data_cod_change())
    }
    
    lt
  })
  
  # ----------------------------------------------------------------------------
  # FIGURE DATA
  
  # Figure data sets
  data_figure4 <- reactive({
    decompose_by_cod(
      L1 = data_lt1(),
      L2 = data_lt2(),
      C1 = data_cod1(),
      C2 = data_cod2()
    )
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
      L1 = data_lt2(),
      L2 = data_lt1(),
      age = c(0, 45, 65),
      perc = input$perc
    )
  })
  
  # Figure 3 - The COD Distribution
  output$figure3 <- renderPlot({
    if (input$mode == "cntr_compare") {
      plot_cod(data_cod(), perc = input$perc)
      
    } else if (input$mode == 'cod_change') {
      plot_cod(data_cod2(), perc = input$perc)
    }
  })
  
  # Figure 4 - The Decomposition
  output$figure4 <- renderPlot(
    plot_decompose(data_figure4(), perc = input$perc)
  )
  
  # ----------------------------------------------------------------------------
  # EVENTS
  observeEvent(input$cod_target, {
    
    if(any(input$cod_target == "ALL")) {
      updatePrettyCheckboxGroup(
        session,
        inputId = "cod_target",
        selected = levels(data_gbd2019_cod$cause_name)
      )}
  })
  
}






