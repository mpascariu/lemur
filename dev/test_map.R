# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed Jun 30 15:27:17 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)
library(shiny)
library(rnaturalearth)
library(sf)
library(fs)
library(leaflet)


world_map_raw <- rnaturalearth::ne_countries(
  scale = "small", 
  type = "countries", 
  returnclass = "sf")

sf_world <- world_map_raw %>% 
  select(country = name_long, iso_a3, iso_a2, pop_est) %>% 
  mutate(continent = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent"),
         region = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region")) %>% 
  filter(str_detect(country, "Antarctic", negate = TRUE)) %>%
  cbind(st_coordinates(st_centroid(., of_largest_polygon = TRUE))) %>% 
  rename(lon = X, lat = Y)

reg = "Romania"

plot_map <- function(reg) {
  
  tag.map.title <- tags$style(
    HTML("
   .leaflet-control.map-title { 
     text-align: left;
     padding-left: 2px; 
     padding-right: 2px; 
     color: rgba(85, 85, 85);
     font-size: 12px;
     font-family: Arial;
     background: rgba(255,255,255,0.8);
     box-shadow: 0 0 15px rgba(0,0,0,0.2);
     border-radius: 5px;
   }
  ")
  )
  
  tooltip <- glue::glue_data(
    sf_world,
    "<b>{country}</b><br>
  Population: {scales::number(pop_est, accuracy = 1)}<br>
  Life Expectancy - females: {scales::number(85, accuracy = 1)}<br>
  Life Expectancy - males: {scales::number(85.5, accuracy = 1)}<br>
  "
  ) %>% 
    purrr::map(htmltools::HTML)
  
  dt <- sf_world %>% 
    filter(country == reg)
  
  leaflet() %>%
    addTiles() %>%
    addMapPane(name = "choropleth", zIndex = 410) %>%
    addMapPane(name = "polygons", zIndex = 420) %>%
    addMapPane(name = "borders", zIndex = 430) %>%
    addMapPane(name = "place_labels", zIndex = 450) %>%
    addProviderTiles("CartoDB.PositronOnlyLabels",
                     group = "Place Labels",
                     options = leafletOptions(pane = "place_labels")) %>%
    addScaleBar(position = "bottomleft") %>%
    addControl(
      tags$div(tag.map.title, HTML("click country on<br>map to filter")),
      position = "topright",
      className = "map-title",
      layerId = "title") %>%
    leaflet.extras::addFullscreenControl(position = "topleft") %>%
    leaflet.extras::addResetMapButton() %>%
    addPolygons(
      data = dt, 
      weight = 2, 
      fillColor = "yellow") %>% 
    addPolygons(
      data = sf_world,
      label = tooltip,
      color = "white", 
      weight = 0.1, 
      smoothFactor = .1,
      opacity = 1, 
      fillOpacity = .5,
      fillColor = ~colorQuantile("YlOrRd", pop_est)(pop_est),
      highlightOptions = highlightOptions(
        color = "white", 
        weight = 2,
        bringToFront = TRUE)) %>% 
    setView(lng = dt$lon, lat = dt$lat, zoom = 5)
  
}


# ----------------------------------------------------------------------------

ui <- fluidPage(
  selectInput(
    inputId  = "region1",
    label    = "Region",
    choices  = c("Romania", "Spain"),
    selected = "Romania",
    width    = "100%"
  ), 
  leafletOutput("map1")
)



server <- function(input, output, session) {
  output$map1 <- renderLeaflet(
    plot_map(input$region1)
  )


}

app <- shinyApp(ui, server)
if (interactive()) app




# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# App 2

library(shiny)

ui <- fluidPage(
  leafletOutput("map1")
)

map <- leaflet() %>% addCircleMarkers(
  lng = runif(10),
  lat = runif(10),
  layerId = paste0("marker", 1:10))
server <- function(input, output, session) {
  output$map1 <- renderLeaflet(map)
  
  observeEvent(input$map1_marker_click, {
    leafletProxy("map1", session) %>%
      removeMarker(input$map1_marker_click$id)
  })
}

app <- shinyApp(ui, server)
if (interactive()) app


