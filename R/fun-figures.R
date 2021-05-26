# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed May 26 19:32:04 2021
# --------------------------------------------------- #


# ----------------------------------------------------------------------------
# Figure 1.

#' Plot map
#' 
#' @examples 
#' plot_map()
#' @export  
plot_map <- function() {
  
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
}

# ----------------------------------------------------------------------------
# Figure 2.

#' Plot function for COD data
#' @inheritParams modify_life_table
#' @param perc Logical. If TRUE data will be displayed as percentages else
#' as absolute values. Default: TRUE.
#' @examples 
#' D <- data_gbd2019_cod # cod data
#' cod <- D[D$region == "Romania" & D$sex == "both" & D$level == "median", ]
#' plot_cod(cod)
#' @export
plot_cod <- function(cod, perc = TRUE) {
  
  # Data preparation
  dt <- cod %>% 
    # compute percentages of each disease for
    # given age-region-period-sex and across ages
    group_by(region, period, sex, cause_name, level) %>%
    summarise(value = sum(deaths)) %>% 
    arrange(value) %>% 
    ungroup() %>% 
    mutate(cause_name  = forcats::as_factor(cause_name))
  
  x_lab = "Number of Deaths"
  
  if (perc) {
    dt <- dt %>% 
      mutate(
        cause_name  = forcats::as_factor(cause_name),
        value = value / sum(value) * 100,
      )
    
    x_lab <- "Percentage [%]" 
  }
  
  # ggplot
  p <- dt %>% 
    ggplot(
      aes(
        y = cause_name,
        x = value,
        fill = cause_name)) + 
    geom_bar(
      stat = "identity",
      width = 0.9,
      position = position_stack(reverse = FALSE)) +
    scale_x_continuous(
      trans = "identity",
      labels = scales::label_number_si(accuracy = 1)) +
    labs(
      x = x_lab,
      y = "Cause of Death",
      size = 10) +
    theme_custom() + 
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "none"
    )
  
  # exit
  return(p)
}

# ----------------------------------------------------------------------------
# Figure 3.



# ----------------------------------------------------------------------------
# Figure 4.

#' Plot function for decompose
#' 
#' @param object An object of class decompose
#' @param ... Arguments to be passed to methods, such as graphical
#' @seealso 
#' \code{\link{decompose_by_cod}}
#' \code{\link{decompose_by_age}}
#' @examples 
#' # See example in the ?decompose_by_cod or ?decompose_by_age help pages
#' @export
plot_decompose <- function(object, ...) {
  
  # Define the aesthetics
  if("cause_name" %in% names(object)) {
    aess <- aes(
      x = x.int,
      y = decomposition, 
      fill = cause_name)
  } else {
    aess <- aes(
      x = x.int,
      y = decomposition)
  }
  
  # Build the plot
  p <- object %>% 
    ggplot(aess) +
    geom_bar(
      stat = "identity",
      width = 0.9,
      position = position_stack(reverse = FALSE)) +
    geom_hline(yintercept = 0) + 
    labs(
      size = 10) +
    theme_custom()
  
  # Exit
  return(p)
}





# ----------------------------------------------------------------------------
# Extras

#' ggplot custom theme
#' @keywords internal 
theme_custom <- function() {
  theme_light() + 
    theme(
      plot.margin = margin(25, 10, 10, 20),
      strip.text.x = element_text(size = 12, colour = "black", face = "bold"),
      strip.background = element_rect(fill = "gray87"),
      text = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}