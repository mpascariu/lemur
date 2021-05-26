# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Thu May 13 16:02:01 2021
# --------------------------------------------------- #

#' Server module: MAP
#' @param input input
#' @param output output
#' @param session session
#' 
#' @rdname mod_map
mod_map_server <- function(input, output, session) {
  ns <- session$ns
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addMapPane(name = "choropleth", zIndex = 410) %>%
      addMapPane(name = "polygons", zIndex = 420) %>%
      addMapPane(name = "borders", zIndex = 430) %>%
      addMapPane(name = "circles", zIndex = 440) %>%
      addMapPane(name = "place_labels", zIndex = 450) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addProviderTiles("CartoDB.PositronOnlyLabels",
                       group = "Place Labels",
                       options = leafletOptions(pane = "place_labels")) %>%
      setView(0, 40, zoom = 1) %>%
      addScaleBar(position = "bottomleft") %>%
      # addControl(
      #   tags$div(tag.map.title, HTML("click country on<br>map to filter")),
      #   position = "bottomleft", 
      #   className = "map-title", 
      #   layerId = "title") %>%
      leaflet.extras::addFullscreenControl(position = "topleft") %>%
      leaflet.extras::addResetMapButton() %>%
      addLayersControl(
        # baseGroups = c("Labels", "No Labels"),
        overlayGroups = c("Place Labels", "Trends", "Cases/Deaths"),
        position = "topleft"
      )
    
  })
  

  chart4_data <- reactive({
    L <- MortalityCauses::data_gbd2019_lt  # life tables
    D <- MortalityCauses::data_gbd2019_cod # cod data
    
    # Select two Life Tables
    region1 = "Romania"
    region2 = "Mexico"
    sex     = "male"
    level   = "median"
    
    lt1 <- L[L$region == region1 & L$sex == sex & L$level == level, ]
    lt2 <- L[L$region == region2 & L$sex == sex & L$level == level, ]
    
    # Select COD corresponding data
    cod1 <- D[D$region == region1 & D$sex == sex & D$level == level, ]
    cod2 <- D[D$region == region2 & D$sex == sex & D$level == level, ]
    
    ## Example of decomposition by age and cause of death
    dec  <- decompose_by_cod(L1 = lt1,
                             L2 = lt2,
                             C1 = cod1,
                             C2 = cod2)
    dec
  })
  
  output$chart4_decomposition <- renderPlot(
    plot_decompose(chart4_data)
  )

}

