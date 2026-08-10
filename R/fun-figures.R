# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Wed Oct  1 22:48:21 2025
# ------------------------------------------------- #

# Figure 1.

#' Plot map
#' @param location Geographical location
#' @param zoom The zoom level
#' @param data data
#' @examples
#' plot_map(location = "Mexico")
#'
#' @export
plot_map <- function(location,
                     zoom = 5,
                     data = lemur::data_sf) {

  check_null(data, "map data")
  
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
  Population: {number(population_2021, accuracy = 1)}<br>
  Life Expectancy - Females: {number(e0f_2021, accuracy = 0.1)}<br>
  Life Expectancy - Males: {number(e0m_2021, accuracy = 0.1)}<br>
  Total Fertility Rate: {number(tfr_2021, accuracy = 0.01)}<br>
  <i>(Year: 2021)</i><br>
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
      fillOpacity  = .25,
      fillColor    = ~ colorQuantile("YlOrRd", e0f_2021)(e0f_2021),
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

  check_null(L1, "Life Table 1")
  check_null(L2, "Life Table 2")
  
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
  
  if (L1$region[1] != L2$region[1] | (L1$region[1] == L2$region[1] & L1$sex[1] != L2$sex[1])) {
    label_losses = "<--- Negative gap"
    label_gains  = "Positive gap --->"
  } else {
    label_losses = "<--- Losses"
    label_gains  = "Gains --->"
  }
  
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
      linewidth = 0.2) +
    geom_point(
      size = 2) +
    geom_vline(
      xintercept = 0,
      linewidth  = 0.8) +
    geom_text(
      x = min(-0.01, (-dmax * 1.05)/2), 
      y = 110, 
      label = label_losses,
      color = "black") + 
    geom_text(
      x = max(0.01, (dmax * 1.05)/2), 
      y = 110, 
      label = label_gains,
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
#' #' @examples
#' D <- data_gbd2021_cod # cod data
#' cod <- D[D$region == "Romania" & D$sex == "both" & D$period == 2021, ]
#' plot_cod(cod)
#' @export
plot_cod <- function(cod, perc = FALSE, type = "barplot") {
  
  check_null(cod, "cod data")
  
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

  check_null(object, "decomposition data")
  
  region = period = sex = COD = cause_name = sign_ <- NULL
  x = x.int = `Age Interval` = `Change in LE` = decomposition <- NULL

  if (!("cause_name" %in% names(object))) {
    by = "age"
  }
  
  object <- rename(object, `Age Interval` = x.int)

  levels(object$`Age Interval`) <- 
    c("0", 
      "1",
      "2-4",
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
# Native plotly versions of Figures 2-4.
#
# The Shiny app used to build these figures with ggplot2 and then convert them
# to plotly with ggplotly(). That conversion is by far the most expensive part
# of the figure pipeline. The functions below build the plotly object directly
# with plot_ly(), skipping the conversion entirely. They are internal to the
# package - the exported plot_*() functions above remain the public API. The
# data preparation in each mirrors the ggplot version so the two look alike.

#' Native plotly version of plot_change (app internal)
#' @inheritParams plot_change
#' @param xlab,ylab Axis titles. If \code{NULL} the same defaults as
#'   \code{\link{plot_change}} are used.
#' @keywords internal
plotly_change <- function(L1, L2,
                          age = seq(0, 110, by = 10),
                          perc = FALSE,
                          xlab = NULL,
                          ylab = NULL) {

  check_null(L1, "Life Table 1")
  check_null(L2, "Life Table 2")

  x = ex = value = col <- NULL

  cols <- c("black", "red", "green")

  # Data ------- (mirror of plot_change)
  d <- L1 %>%
    mutate(
      value = ex - L2$ex,
      col = "black",
      col = replace(col, value < -0.0001, "red"),
      col = replace(col, value >  0.0001, "green"),
      col = factor(col, cols)
    ) %>%
    filter(x %in% age)

  if (perc) {
    d <- mutate(d, value = value/ex * 100)
    if (is.null(xlab)) xlab <- "Difference in Life Expectancy\n[%]"
  } else {
    if (is.null(xlab)) xlab <- "Difference in Life Expectancy\n(Years)"
  }
  if (is.null(ylab)) ylab <- "Age\n(Years)"

  dmax <- max(abs(d$value))
  d <- d %>% mutate(value = round(value, 3))

  if (L1$region[1] != L2$region[1] |
      (L1$region[1] == L2$region[1] & L1$sex[1] != L2$sex[1])) {
    label_losses = "<--- Negative gap"
    label_gains  = "Positive gap --->"
  } else {
    label_losses = "<--- Losses"
    label_gains  = "Gains --->"
  }

  # x-axis tick labels, SI formatted like the ggplot version
  xbr  <- pretty(c(-dmax, dmax), n = 6)
  xtic <- label_number_si(accuracy = 0.01)(xbr)

  # Vertical line at x = 0 plus one dashed segment per age, mirroring
  # geom_vline(xintercept = 0) + geom_segment(linetype = 2, color = 1).
  # The segments are drawn layer = "below" so the markers overlay them
  # (matching geom_segment -> geom_point ordering in the ggplot version).
  seg_shapes <- lapply(seq_len(nrow(d)), function(i) {
    list(
      type = "line",
      layer = "below",
      x0 = 0, x1 = d$value[i],
      y0 = d$x[i], y1 = d$x[i],
      line = list(color = "black", dash = "dot", width = 1)
    )
  })
  shapes <- c(
    list(list(
      type = "line",
      x0 = 0, x1 = 0,
      y0 = min(d$x), y1 = max(d$x),
      line = list(color = "black", width = 0.8)
    )),
    seg_shapes
  )

  p <- plotly::plot_ly(
    data = d,
    x = ~value,
    y = ~x,
    type = "scatter",
    mode = "markers",
    color = ~col,
    colors = cols,
    marker = list(size = 14),
    hoverinfo = "x+y",
    showlegend = FALSE
  ) %>%
    plotly::layout(
      shapes = shapes,
      xaxis = list(
        title     = xlab,
        titlefont = list(size = 13),
        tickfont  = list(size = 11),
        range     = c(-dmax, dmax) * 1.05,
        tickvals  = xbr,
        ticktext  = xtic
      ),
      yaxis = list(
        title     = ylab,
        titlefont = list(size = 14),
        tickfont  = list(size = 11),
        range     = c(min(d$x) - 1, max(d$x) + 2)
      ),
      annotations = list(
        list(
          x = min(-0.01, (-dmax * 1.05)/2),
          y = max(d$x),
          text = label_losses,
          showarrow = FALSE,
          xref = "x", yref = "y",
          font = list(size = 12)
        ),
        list(
          x = max(0.01, (dmax * 1.05)/2),
          y = max(d$x),
          text = label_gains,
          showarrow = FALSE,
          xref = "x", yref = "y",
          font = list(size = 12)
        )
      )
    )

  return(p)
}


#' Native plotly version of plot_cod (app internal)
#' @inheritParams plot_cod
#' @param xlab X axis title. If \code{NULL} the same defaults as
#'   \code{\link{plot_cod}} are used.
#' @param mode One of "mode_cod", "mode_cntr", "mode_sex", "mode_sdg",
#'   "mode_sdg2" - passed through from the app to decide whether the data
#'   spans two regions or two sexes and needs a two-panel subplot (the
#'   native equivalent of facet_wrap()).
#' @keywords internal
plotly_cod <- function(cod,
                       perc = FALSE,
                       xlab = NULL,
                       mode = "mode_cod") {

  check_null(cod, "cod data")

  region = period = sex = cause_name = Deaths = COD <- NULL

  # Data preparation (mirror of plot_cod)
  dt <- cod %>%
    group_by(region, period, sex, cause_name) %>%
    summarise(Deaths = sum(deaths)) %>%
    mutate(sex = toupper(sex)) %>%
    rename(COD = cause_name) %>%
    arrange(Deaths) %>%
    ungroup()

  if (perc) {
    dt <- dt %>%
      group_by(region, sex) %>%
      mutate(
        Deaths = Deaths / sum(Deaths) * 100,
        Deaths = round(Deaths, 2)) %>%
      ungroup()
    if (is.null(xlab)) xlab <- "Proportion of the Total No. of Deaths\n[%]"
  } else {
    dt <- dt %>%
      mutate(Deaths = round(Deaths, 0)) %>%
      ungroup()
    if (is.null(xlab)) xlab <- "Number of Deaths\n"
  }

  # y axis order: the data is sorted by ascending Deaths, then autorange is
  # reversed below so the largest cause is displayed on top (the native
  # equivalent of scale_y_discrete(limits = rev)). unique() is needed because
  # in the comparison modes each cause appears once per region/sex.
  dt$COD <- factor(dt$COD, levels = unique(dt$COD[order(dt$Deaths)]))

  xbr  <- pretty(c(0, max(dt$Deaths)), n = 5)
  xtic <- label_number_si(accuracy = 1)(xbr)

  one_plot <- function(sub) {
    plotly::plot_ly(
      data = sub,
      x = ~Deaths,
      y = ~COD,
      color = ~COD,
      colors = glasbey(),
      type = "bar",
      orientation = "h",
      showlegend = FALSE,
      hoverinfo = "x+name"
    )
  }

  faceted <- mode %in% c("mode_cntr", "mode_sex")

  if (faceted) {
    g   <- if (mode == "mode_cntr") "region" else "sex"
    spl <- lapply(split(dt, dt[[g]]), one_plot)
    p   <- plotly::subplot(spl, shareY = TRUE, nrows = 1, margin = 0.04)
    p   <- p %>%
      plotly::layout(
        barmode = "stack",
        xaxis = list(
          title     = xlab,
          titlefont = list(size = 14),
          tickfont  = list(size = 11),
          tickvals  = xbr,
          ticktext  = xtic
        ),
        xaxis2 = list(
          tickfont = list(size = 11),
          tickvals = xbr,
          ticktext = xtic
        ),
        yaxis = list(
          title = "",
          titlefont = list(size = 14),
          tickfont  = list(size = 11),
          autorange = "reversed",
          # Keep the COD labels a little off the y-axis line. The plotly build
          # bundled with plotly 4.12.1 does not support ticklabelstandoff, so
          # pad with a couple of non-breaking spaces appended after each label
          # (NBSP survives SVG whitespace trimming; regular spaces do not).
          ticksuffix = "  "
        )
      )

  } else {
    p <- one_plot(dt) %>%
      plotly::layout(
        barmode = "stack",
        xaxis = list(
          title     = xlab,
          titlefont = list(size = 14),
          tickfont  = list(size = 11),
          tickvals  = xbr,
          ticktext  = xtic
        ),
        yaxis = list(
          title = "",
          titlefont = list(size = 14),
          tickfont  = list(size = 11),
          autorange = "reversed",
          # Keep the COD labels a little off the y-axis line. The plotly build
          # bundled with plotly 4.12.1 does not support ticklabelstandoff, so
          # pad with a couple of non-breaking spaces appended after each label
          # (NBSP survives SVG whitespace trimming; regular spaces do not).
          ticksuffix = "  "
        )
      )
  }

  return(p)
}


#' Native plotly version of plot_decompose (app internal)
#' @inheritParams plot_decompose
#' @param xlab,ylab Axis titles. If \code{NULL} the same defaults as
#'   \code{\link{plot_decompose}} are used.
#' @param ttip Hover/tooltip fields, as returned by the app caption system
#'   (e.g. \code{c("fill", "y")}). Used to select hoverinfo.
#' @keywords internal
plotly_decompose <- function(object,
                             perc = FALSE,
                             by = "both",
                             xlab = NULL,
                             ylab = NULL,
                             ttip = c("fill", "y")) {

  check_null(object, "decomposition data")

  region = period = sex = COD = cause_name = sign_ <- NULL
  x = x.int = `Age Interval` = `Change in LE` = decomposition <- NULL

  if (!("cause_name" %in% names(object))) {
    by = "age"
  }

  object <- rename(object, `Age Interval` = x.int)

  levels(object$`Age Interval`) <-
    c("0",
      "1",
      "2-4",
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

  # input data (mirror of plot_decompose)
  if (by == "age") {
    object <- object %>%
      group_by(region, period, sex, `Age Interval`, x) %>%
      summarise(decomposition = sum(decomposition)) %>%
      ungroup()

  } else if (by == "cod") {
    object <- object %>%
      rename(COD = cause_name) %>%
      group_by(region, period, sex, COD) %>%
      summarise(decomposition = sum(decomposition)) %>%
      ungroup()

  } else {
    object <- rename(object, COD = cause_name)
  }

  # compute % is necessary
  if (perc) {
    if (is.null(ylab)) ylab <- "Change in Life Expectancy at Birth\n[%]"
    d <- object %>%
      mutate(
        sign_ = sign(decomposition),
        `Change in LE` = decomposition / sum(decomposition),
        `Change in LE` = abs(`Change in LE`) * sign_,
        `Change in LE` = round(`Change in LE`, 4)
      )

  } else {
    if (is.null(ylab)) ylab <- "Change in Life Expectancy at Birth\n(Years)"
    d <- object %>%
      rename(`Change in LE` = decomposition) %>%
      mutate(`Change in LE` = round(`Change in LE`, 4))
  }

  # plotly draws categorical axes in order of appearance, so make sure the
  # age groups stay ordered by the numeric age (ggplot orders by factor level)
  if (by != "cod") {
    d <- d %>% arrange(x)
  }

  # tooltip selection
  if (by == "age") {
    hov <- "x+y"
  } else {
    hov <- paste(unique(c(ttip[ttip %in% c("x", "y")], "name")), collapse = "+")
  }

  if (by == "cod") {
    if (is.null(xlab)) xlab <- "Causes of Death\nDecomposition"
    p <- plotly::plot_ly(
      data = d,
      x = ~`Change in LE`,
      y = ~sex,
      color = ~COD,
      colors = glasbey(),
      type = "bar",
      orientation = "h",
      showlegend = FALSE,
      hoverinfo = hov
    ) %>%
      plotly::layout(
        barmode = "stack",
        shapes = list(list(
          type = "line",
          x0 = 0, x1 = 0,
          y0 = -0.5, y1 = max(0.5, length(unique(d$sex)) - 0.5),
          line = list(color = "black", width = 1)
        )),
        xaxis = list(
          title     = xlab,
          titlefont = list(size = 14),
          tickfont  = list(size = 11)
        ),
        yaxis = list(
          title = "",
          titlefont = list(size = 14),
          tickfont  = list(size = 11),
          showticklabels = FALSE
        )
      )

  } else {
    if (is.null(xlab)) xlab <- "Age Group\n(Years)"
    if (by == "both") {
      # color = ~COD makes plotly split the data into one stacked trace per
      # cause, equivalent to the ggplot fill = COD mapping.
      p <- plotly::plot_ly(
        data = d,
        x = ~`Age Interval`,
        y = ~`Change in LE`,
        type = "bar",
        color = ~COD,
        colors = glasbey(),
        showlegend = FALSE,
        hoverinfo = hov
      )
    } else {
      p <- plotly::plot_ly(
        data = d,
        x = ~`Age Interval`,
        y = ~`Change in LE`,
        type = "bar",
        showlegend = FALSE,
        hoverinfo = hov
      )
    }
    p <- p %>%
      plotly::layout(
        barmode = "stack",
        shapes = list(list(
          type = "line",
          x0 = -0.5, x1 = length(levels(d$`Age Interval`)) - 0.5,
          y0 = 0, y1 = 0,
          line = list(color = "black", width = 1)
        )),
        xaxis = list(
          title     = xlab,
          titlefont = list(size = 14),
          tickfont  = list(size = 11),
          tickangle = -45
        ),
        yaxis = list(
          title     = ylab,
          titlefont = list(size = 14),
          tickfont  = list(size = 11)
        )
      )
  }

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

#' @keywords internal
check_null <- function(data, data_name = "data") {
  if (is.null(data)) {
    stop(paste(data_name, "is NULL - cannot proceed with plotting"))
  }
  return(TRUE)
}


