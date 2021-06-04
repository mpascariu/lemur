# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Fri Jun 04 12:02:07 2021
# --------------------------------------------------- #

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

#' Plot the difference in life expectancy of two life tables
#' 
#' @inheritParams decompose_by_cod
#' @inheritParams plot_cod
#' @param age Reference ages.
#' @export
plot_change <- function(L1, L2,
                        age = c(0, 40, 65),
                        perc = FALSE) {
  
  # Data
  d <- L1 %>% 
    mutate(
      value = ex - L2$ex,
      col = ifelse(value < 0, "red", "green")) %>% 
    filter(x %in% age) 
  
  if (perc) {
    d <- mutate(d, value = value/ex * 100)
    xlab <- "Difference in Life Expectancy\n[%]"
    
  } else {
    xlab <- "Difference in Life Expectancy\n(Years)"
    
  }
  
  dmax <- max(abs(d$value))
  
  # Figure
  p <- d %>%   
    ggplot(aes(x = value, y = x, color = col)) + 
    geom_vline(xintercept = 0, size = 1) + 
    geom_segment(
      xend = 0, 
      yend = d$x,
      linetype = 2,
      color = 1,
      size = 0.5) +
    geom_point(size = 6) +
    scale_x_continuous(
      limits = c(-dmax, dmax),
      labels = scales::label_number_si(accuracy = 0.01)) +
    scale_color_manual(values = c("red", "green")) +
    labs(x = xlab,
         y = "Age\n(Years)") + 
    plot_theme()
  
  return(p)
}


# ----------------------------------------------------------------------------
# Figure 3.

#' Plot function for COD data
#' @inheritParams modify_life_table
#' @param perc Logical. If TRUE data will be displayed as percentages else
#' as absolute values. Default: FALSE.
#' @examples 
#' D <- data_gbd2019_cod # cod data
#' cod <- D[D$region == "Romania" & D$sex == "both" & D$level == "median", ]
#' plot_cod(cod)
#' @export
plot_cod <- function(cod, perc = FALSE) {
  
  # Data preparation
  dt <- cod %>% 
    group_by(region, period, sex, cause_name, level) %>%
    summarise(value = sum(deaths)) %>% 
    arrange(value) %>% 
    ungroup()
  
    # compute percentages of each disease for
    # given age-region-period-sex and across ages
  if (perc) {
    dt <- dt %>% 
      group_by(region) %>% 
      mutate(
        value = value / sum(value) * 100,
      ) %>% 
      ungroup()
    
    x_lab <- "Proportion of the Total No. of Deaths\n[%]" 
    
  } else {
    x_lab <- "Number of Deaths\n"
    
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
      y = "Cause of Death") +
    plot_theme()
  
  # exit
  return(p)
}


# ----------------------------------------------------------------------------
# Figure 4.

#' Plot function for decompose
#' 
#' @param object An object of class decompose
#' @inheritParams plot_cod
#' @seealso 
#' \code{\link{decompose_by_cod}}
#' \code{\link{decompose_by_age}}
#' @examples 
#' # See example in the ?decompose_by_cod or ?decompose_by_age help pages
#' @export
plot_decompose <- function(object, perc = FALSE) {
  
  # data
  if (perc) {
    ylab <- "Change in Life Expectancy at Birth\n[%]"
    d <- object %>%
      mutate(
        sign_ = sign(decomposition),
        value = decomposition / sum(decomposition),
        value = abs(value) * sign_
        )
    
  } else {
    ylab <- "Change in Life Expectancy at Birth\n(Years)"
    d <- object %>%
      mutate(value = decomposition)
    
  }
  
  # Define the aesthetics
  if("cause_name" %in% names(d)) {
    aess <- aes(
      x = x.int,
      y = value, 
      fill = cause_name)
  } else {
    aess <- aes(
      x = x.int,
      y = value)
  }
  
  # Build the plot
  p <- d %>% 
    ggplot(aess) +
    geom_bar(
      stat = "identity",
      width = 0.9,
      position = position_stack(reverse = FALSE)) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(
      trans = "identity",
      labels = scales::label_number_si(accuracy = 0.01)) +
    labs(
      x = "Age Group\n(Years)",
      y = ylab
    ) +
    plot_theme() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Exit
  return(p)
}





# ----------------------------------------------------------------------------
# Extras

#' Plot theme
#' \code{ggplot2} custom theme used in the package.
#' @export 
plot_theme <- function() {
  theme_light() + 
    theme(
      axis.title = element_text(size = 12),
      # axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.margin = margin(25, 10, 10, 20),
      strip.text.x = element_text(size = 12, colour = "black", face = "bold"),
      strip.background = element_rect(fill = "gray87"),
      text = element_text(size = 14),
      legend.position = "none"
    )
}