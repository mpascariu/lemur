# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed Jun 30 11:19:05 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)
library(leaflet)

library(rnaturalearth)
library(sf)
library(dplyr)
library(stringr)
library(here)
library(fs)

world_map_raw <- rnaturalearth::ne_countries(scale = "small", type = "countries", returnclass = "sf")

sf_world <- world_map_raw %>% 
  select(country = name_long, iso_a3, iso_a2, pop_est) %>% 
  mutate(continent = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent"),
         region = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region")) %>% 
  filter(str_detect(country, "Antarctic", negate = TRUE)) %>% 
  cbind(st_coordinates(st_centroid(., of_largest_polygon = TRUE))) %>% 
  rename(lon = X, lat = Y)


geo_ref <- countrycode::codelist_panel %>% 
  dplyr::select(iso3c, continent, region, country = country.name.en) %>% 
  dplyr::distinct() %>% 
  tibble::as_tibble()

dim(sf_world)

# usethis::use_data(sf_world, overwrite = TRUE)
# usethis::use_data(geo_ref, overwrite = TRUE)


latest_date = Sys.Date()-1



# make_map <- function(df_trends, sf_world, latest_date = Sys.Date()-1) {
# sf_df <- sf_world %>% 
#   dplyr::select(iso_a3, lon, lat) %>%  
#   dplyr::inner_join(
#     select(df_trends, country, iso_a3, cases, deaths, trend_cases = trend_cases_14d, trend_deaths = trend_deaths_14d), 
#     by = "iso_a3"
#   )

sf_df <- sf_world


sfs <- sf_df %>% 
  dplyr::filter(continent == isolate("Asia"))

sf_df %>% 
  leaflet() %>%
  addPolygons(
    color = "#444444", 
    weight = 1, 
    smoothFactor = .1,
    opacity = 1, 
    fillOpacity = .5,
    fillColor = ~colorQuantile("YlOrRd", pop_est)(pop_est),
    highlightOptions = highlightOptions(
      color = "white", 
      weight = 2,
      bringToFront = TRUE))



tooltip <- glue::glue_data(
  sf_df,
  "<b>{country}</b><br>
  Population: {scales::number(pop_est, accuracy = 1)}<br>
  Life Expectancy - females: {scales::number(85, accuracy = 1)}<br>
  Life Expectancy - males: {scales::number(85.5, accuracy = 1)}<br>
  "
) %>% 
  purrr::map(htmltools::HTML)

lvls <- c("High", "..", "...", ".", "Low")
values <- scico::scico(
  5, 
  palette = "vikO", 
  begin = .2,
  end = .8, 
  direction = -1) %>% 
  set_names(lvls)

pal <- leaflet::colorFactor(
  palette = values, 
  levels = names(values),
  ordered = TRUE, 
  na.color = NA)

pal_bw <- leaflet::colorFactor(
  palette = c("#FFFFFF", rep("#808080", 3), "#FFFFFF"), 
  levels = lvls, 
  ordered = TRUE, 
  na.color = NA)

leaflet(sf_df) %>%
  addMapPane(name = "choropleth", zIndex = 460) %>%
  addMapPane(name = "polygons", zIndex = 470) %>%
  addMapPane(name = "borders", zIndex = 480) %>%
  addMapPane(name = "place_labels", zIndex = 450) %>%
  addTiles(
    # urlTemplate = "https://{s}.basemaps.cartocdn.com/{z}/{x}/{y}{r}.png",
    # urlTemplate = "//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
    urlTemplate = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager_labels_under/{z}/{x}/{y}.png",
    group = "Labels",
    options = leaflet::leafletOptions(pane = "place_labels")
  ) %>%
  setView(0, 40, zoom = 2) %>%
  leaflet.extras::addFullscreenControl(position = "topleft") %>%
  leaflet.extras::addResetMapButton() %>%
  addLayersControl(
    overlayGroups = c("Labels", "Population size", "GDP per capita"),
    position = "topleft",
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addPolygons(
    group = "Population size",
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
  addLegend(
    position = "bottomright",
    pal = pal,
    values = factor(names(values), levels = names(values)),
    # title = paste("Legend"),
    layerId = "choro_legend",
    group = "Population size"
  ) %>% 
  addProviderTiles(
    "CartoDB.PositronOnlyLabels",
    group = "Labels",
    options = leafletOptions(pane = "place_labels")) %>%
  addScaleBar(position = "bottomleft") %>%
  addControl(
    tags$div(tag.map.title, HTML("click country on<br>map to filter")),
    position = "topright",
    className = "map-title",
    layerId = "title")
# }

