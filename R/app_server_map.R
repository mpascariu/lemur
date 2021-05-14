# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Thu May 13 16:02:01 2021
# --------------------------------------------------- #

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
}