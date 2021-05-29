# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Sat May 29 18:21:13 2021
# --------------------------------------------------- #

#' Server module: MAP
#' @keywords internal
mod_map_server <- function(input, output, session) {
  ns <- session$ns
  
  # ----------------------------------------------------------------------------
  # INPUT DATA
  
  # cod data
  data_cod <- reactive(MortalityCauses::data_gbd2019_cod) 
  
  data_cod1 <- reactive({
    data_cod() %>% 
      dplyr::filter(
        region == input$region1,
        sex == input$sex,
        level == "median")
  })
  
  data_cod2 <- reactive({
    data_cod() %>% 
      dplyr::filter(
        region == input$region2,
        sex == input$sex,
        level == "median")
  })
  
  # life tables data
  data_lt  <- reactive(MortalityCauses::data_gbd2019_lt) 
  
  data_lt1 <- reactive({
    data_lt() %>% 
      dplyr::filter(
        region == input$region1,
        sex == input$sex,
        level == "median")
  })
  
  data_lt2 <- reactive({
    data_lt() %>% 
      dplyr::filter(
        region == input$region2,
        sex == input$sex,
        level == "median")
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
  output$figure2 <- renderPlot(
    plot_change(
      L1 = data_lt1(),
      L2 = data_lt2(),
      age = c(0, 45, 65),
      perc = input$perc2
      )
  )
  
  # Figure 3 - The COD Distribution
  output$figure3 <- renderPlot(
    plot_cod(
      data_cod1(),
      perc = input$perc3
    )
  )
  
  # Figure 4 - The Decomposition
  output$figure4 <- renderPlot(
    plot_decompose(
      data_figure4(),
      perc = input$perc4
    )
  )
  
  # outputOptions(output, "figure1", suspendWhenHidden = FALSE)
  # outputOptions(output, "figure2", suspendWhenHidden = FALSE)
  # outputOptions(output, "figure4", suspendWhenHidden = FALSE)
}

