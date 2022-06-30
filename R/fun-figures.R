# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Thu Mar 17 18:14:49 2022
# --------------------------------------------------- #

# Figure 1.

#' Plot map
#' @param location Geographical location
#' @param zoom The zoom level
#' @param data data
#' @examples
#' plot_map(location = "MEXICO")
#'
#' @export
plot_map <- function(location,
                     zoom = 5,
                     data = lemur::data_sf) {

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
    data,
    "<strong>{name}</strong><br>
  Population (2019): {number(pop_2019, accuracy = 1)}<br>
  Life Expectancy - Females: {number(e0F, accuracy = 0.1)}<br>
  Life Expectancy - Males: {number(e0M, accuracy = 0.1)}<br>
  Total Fertility Rate: {number(tfr, accuracy = 0.01)}<br>
  Sex Ratio: {number(sexRatio, accuracy = 0.01)}<br>
  <i>(Source: WPP 2019)</i><br>
  "
  ) %>%
    purrr::map(htmltools::HTML)

  dt <- data[data$name == location, ]

  leaflet() %>%
    addTiles() %>%
    addMapPane(name = "choropleth", zIndex = 410) %>%
    addMapPane(name = "polygons", zIndex = 420) %>%
    addMapPane(name = "borders", zIndex = 430) %>%
    addMapPane(name = "place_labels", zIndex = 450) %>%
    addProviderTiles(
      "CartoDB.PositronOnlyLabels",
      group = "Place Labels",
      options = leafletOptions(pane = "place_labels")) %>%
    addScaleBar(position = "bottomleft") %>%
    leaflet.extras::addFullscreenControl(position = "topleft") %>%
    leaflet.extras::addResetMapButton() %>%
    addPolygons(
      data      = dt,
      weight    = 2,
      fillColor = "yellow") %>%
    addPolygons(
      data         = data,
      label        = tooltip,
      color        = "white",
      weight       = 0.1,
      smoothFactor = .1,
      opacity      = 1,
      fillOpacity  = .5,
      fillColor    = ~ colorQuantile("YlOrRd", e0F)(e0F),
      highlightOptions = highlightOptions(
        color  = "white",
        weight = 2,
        bringToFront = TRUE)) %>%
    setView(
      lng  = dt$lon,
      lat  = dt$lat,
      zoom = zoom)

}



# ----------------------------------------------------------------------------
# Figure 2.

#' Plot the difference in life expectancy of two life tables
#'
#' @inheritParams decompose_by_cod
#' @inheritParams plot_cod
#' @inheritParams ggplot2::labs
#' @param age Reference ages.
#' @export
plot_change <- function(L1, L2,
                        age = seq(0, 110, by = 10),
                        perc = FALSE,
                        title = NULL,
                        subtitle = ""
                        ) {

  x = ex = value = `Life Expectancy Difference` = Age <- NULL

  # Data -------
  cols <- c("black", "red", "green")

  d <- L1 %>%
    mutate(
      value = ex - L2$ex,
      col = "black",
      col = replace(col, value < -0.0001, "red"),
      col = replace(col, value >  0.0001, "green"),
      col = factor(col, cols),
      ) %>%
    filter(x %in% age)

  if (perc) {
    d <- mutate(d, value = value/ex * 100)
    xlab <- "Difference in Life Expectancy\n[%]"

  } else {
    xlab <- "Difference in Life Expectancy\n(Years)"

  }

  dmax <- max(abs(d$value))
  d <- d %>%
    mutate(
      value = round(value, 3)) %>%
    rename(
      `Life Expectancy Difference` = value,
      Age = x)
  
  # -------------
  # Figure
  
  p <- d %>%
    ggplot(aes(
      x     = `Life Expectancy Difference`,
      y     = Age,
      color = col)) +
    geom_segment(
      xend     = 0,
      yend     = d$Age,
      linetype = 2,
      color    = 1,
      size     = 0.2) +
    geom_point(
      size = 2) +
    geom_vline(
      xintercept = 0,
      size       = 0.8) +
    geom_text(
      x = min(-0.01, (-dmax * 1.05)/2), 
      y = 110, 
      label = "<--- Losses",
      color = "black") + 
    geom_text(
      x = max(0.01, (dmax * 1.05)/2), 
      y = 110, 
      label = "Gains --->",
      color = "black") + 
    scale_x_continuous(
      limits = c(-dmax, dmax) * 1.05,
      labels = label_number_si(accuracy = 0.01)) +
    scale_color_manual(
      name   = "",
      values = cols,
      drop   = FALSE
    ) +
    labs(
      title    = title,
      subtitle = subtitle,
      x        = xlab,
      y        = "Age\n(Years)") +
    plot_theme()

  return(p)
}


# ----------------------------------------------------------------------------
# Figure 3.

#' Plot function for COD data
#' @inheritParams modify_life_table
#' @param perc Logical. If TRUE data will be displayed as percentages else
#' as absolute values. Default: FALSE.
#' @param type Options: "barplot" or "piechart".
#' @export
plot_cod <- function(cod, perc = FALSE, type = "barplot") {

  region = period = sex = cause_name <- NULL
  deaths = Deaths = COD <- NULL

  # Data preparation
  dt <- cod %>%
    group_by(
      region,
      period,
      sex,
      cause_name) %>%
    summarise(Deaths = sum(deaths)) %>%
    mutate(sex = toupper(sex)) %>%
    rename(COD = cause_name) %>%
    arrange(Deaths) %>%
    ungroup()

  # compute percentages of each disease for
  # given age-region-period-sex and across ages
  if (perc) {
    dt <- dt %>%
      group_by(
        region,
        sex) %>%
      mutate(
        Deaths = Deaths / sum(Deaths) * 100,
        Deaths = round(Deaths, 2)
      ) %>%
      ungroup()

    x_lab <- "Proportion of the Total No. of Deaths\n[%]"

  } else {
    dt <- dt %>% mutate(
      Deaths = round(Deaths, 0)
      ) %>%
      ungroup()

    x_lab <- "Number of Deaths\n"

  }


  # Define the aesthetics

  if (type == "barplot") {
    p <- dt %>%
      ggplot(
        aes(x = Deaths, y = COD, fill = COD)
      ) +
      geom_bar(
        stat = "identity",
        width = 0.9,
        position = position_stack(reverse = FALSE)) +
      scale_x_continuous(
        trans = "identity",
        labels = label_number_si(accuracy = 1)) +
      plot_theme()

  } else if (type == "piechart") {
    p <- dt %>%
      ggplot(
        aes(x = "", y = Deaths, fill = COD)) +
      geom_bar(
        stat = "identity",
        width = 0.9,
        color = "white") +
      coord_polar("y", start=0) +
      scale_y_continuous(
        trans = "identity",
        labels = label_number_si(accuracy = 1)) +
      plot_theme() +
      theme(legend.position = "right")

  }

  # ggplot
  p <- p +
    scale_fill_manual(
      name = "",
      values = glasbey(),
      drop = FALSE
    ) +
    labs(
      x = x_lab,
      y = "")

  # exit
  return(p)
}



# ----------------------------------------------------------------------------
# Figure 4.

#' Plot function for decompose
#'
#' @param object An object of class decompose
#' @param by dimensions on which to build the plot.
#' Options: "both", "age", "cod".
#' @inheritParams plot_cod
#' @seealso
#' \code{\link{decompose_by_cod}}
#' \code{\link{decompose_by_age}}
#' @examples
#' # See example in the ?decompose_by_cod or ?decompose_by_age help pages
#' @export
plot_decompose <- function(object,
                           perc = FALSE,
                           by = "both") {

  region = period = sex = COD = cause_name = sign_ <- NULL
  x = x.int = `Age Interval` = `Change in LE` = decomposition <- NULL

  if (!("cause_name" %in% names(object))) {
    by = "age"
  }
  
  object <- rename(object, `Age Interval` = x.int)

  levels(object$`Age Interval`) <- 
    c("0", 
      "1-4",
      "5-9",
      "10-14",
      "15-19",
      "20-24",
      "25-29",
      "30-34",
      "35-39",
      "40-44",
      "45-49",
      "50-54",
      "55-59",
      "60-64",
      "65-69",
      "70-74",
      "75-79",
      "80-84",
      "85-89",
      "90-94",
      "95-99",
      "100-104",
      "105-109",
      "+110")

  # input data
  if(by == "age") {
    object <- object %>%
      group_by(
        region,
        period,
        sex,
        `Age Interval`,
        x) %>%
      summarise(decomposition = sum(decomposition)) %>%
      ungroup()

  } else if (by == "cod") {
    object <- object %>%
      rename(COD = cause_name) %>%
      group_by(
        region,
        period,
        sex,
        COD) %>%
      summarise(decomposition = sum(decomposition)) %>%
      ungroup()

  } else {
    object <- rename(object, COD = cause_name)

  }
  # compute % is necessary
  if (perc) {
    ylab <- "Change in Life Expectancy at Birth\n[%]"
    d <- object %>%
      mutate(
        sign_ = sign(decomposition),
        `Change in LE` = decomposition / sum(decomposition),
        `Change in LE` = abs(`Change in LE`) * sign_,
        `Change in LE` = round(`Change in LE`, 4)
      )

  } else {
    ylab <- "Change in Life Expectancy at Birth\n(Years)"
    d <- object %>%
      rename(`Change in LE` = decomposition) %>%
      mutate(`Change in LE` = round(`Change in LE`, 4))

  }

  # Define the aesthetics
  if(by == "both") {
    aess <- aes(x = `Age Interval`, y = `Change in LE`, fill = COD)
    xlab <- "Age Group\n(Years)"

  } else if (by == "age") {
    aess <- aes(x = `Age Interval`, y = `Change in LE`)
    xlab <- "Age Group\n(Years)"

  } else {
    # aess <- aes(x = COD, y = `Change in LE`, fill = COD)
    aess <- aes(y = sex, x = `Change in LE`, fill = COD)
    xlab <- "Causes of Death\nDecomposition"
  }

  if (by == "cod") {
    p <- d %>%
      ggplot(aess) +
      geom_bar(
        position = "stack",
        stat = "identity" ,
        width = 0.5) +
      geom_vline(xintercept = 0) +
      scale_x_continuous(
        trans = "identity",
        labels = label_number_si(accuracy = 0.01)) +
      plot_theme() +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        )

  } else {

    p <- d %>%
      ggplot(aess) +
      geom_bar(
        stat = "identity",
        width = 0.9,
        position = position_stack(reverse = FALSE)) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(
        trans = "identity",
        labels = label_number_si(accuracy = 0.01)) +
      plot_theme() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )

  }

  p <- p +
    scale_fill_manual(
      name = "",
      values = glasbey(),
      drop = FALSE
    ) +
    labs(
      x = xlab,
      y = ylab
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
      axis.title       = element_text(size = 12, colour = "black", face = "bold"),
      axis.text        = element_text(size = 12, colour = "black"),
      plot.margin      = margin(0, 5, 1, 10),
      text             = element_text(size = 14),
      legend.position  = "none",
      strip.text.x     = element_text(size = 12, colour = "black", face = "bold"),
      strip.background = element_rect(fill = "gray87"),
    )
}

#' glasbey color palette - rearranged
#' pals::glasbey()
#' @keywords internal
glasbey <- function() {
  c(
    "#000033", 
    "#0000FF",
    "#FF0000", 
    "#FF00B6",
    "#A10300",
    "#FFD300",
    "#783FC1",
    "#005300",
    "#00FF00",
    "#02AD24",
    "#14F9FF",
    "#1F9698",
    "#201A01",
    "#720055",
    "#766C95",
    "#FE8F42",
    "#858567",
    "#886C00",
    "#93D4FF",
    "#9A4D42",
    "#B1CC71",
    "#C8FF00",
    "#DC5E93",
    "#DD00FF",
    "#F1085C",
    "#F2F318",
    "#FFACFD",
    "#FFB79F",
    "#00479E",
    "#004CFF"
  )
}



