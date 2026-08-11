
# Figure 1.

#' Plot an interactive map
#' @param location Geographical location.
#' @param zoom The zoom level.
#' @param data An \code{sf} object with the map polygons.
#' Default: \code{lemur::data_sf}.
#' @return A leaflet widget.
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

  tooltip <- sprintf(
    "<strong>%s</strong><br>
  Population: %s<br>
  Life Expectancy - Females: %s<br>
  Life Expectancy - Males: %s<br>
  Total Fertility Rate: %s<br>
  <i>(Year: 2021)</i><br>
  ",
    data$name,
    number(data$population_2021, accuracy = 1),
    number(data$e0f_2021, accuracy = 0.1),
    number(data$e0m_2021, accuracy = 0.1),
    number(data$tfr_2021, accuracy = 0.01)
  ) %>%
    lapply(HTML)

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
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param age Reference ages.
#' @return A plotly widget.
#' @export
plot_change <- function(L1, L2,
                        age = seq(0, 110, by = 10),
                        perc = FALSE,
                        title = NULL,
                        subtitle = "") {

  check_null(L1, "Life Table 1")
  check_null(L2, "Life Table 2")

  # Native plotly figure (same data prep as the ggplot version this
  # function replaced)
  p <- plotly_change(L1, L2, age = age, perc = perc)

  # Title
  if (!is.null(title)) {
    p <- plotly::layout(p, title = list(text = title))
  }

  # Subtitle: a paper-referenced annotation. plotly::layout(annotations = ...)
  # replaces existing annotations, so append to the ones already set by
  # plotly_change (label_losses / label_gains) instead of overwriting them.
  if (nzchar(subtitle)) {
    sub_ann <- list(
      x = 0, y = 1.0,
      xref = "paper", yref = "paper",
      xanchor = "left", yanchor = "top",
      text = subtitle,
      showarrow = FALSE,
      font = list(size = 12)
    )
    p$x$layout$annotations <- c(p$x$layout$annotations, list(sub_ann))
  }

  return(p)
}


# ----------------------------------------------------------------------------
# Figure 3.

#' Plot function for COD data
#' @inheritParams modify_life_table
#' @param perc Logical. If TRUE data will be displayed as percentages else
#' as absolute values. Default: FALSE.
#' @param type Options: "barplot" or "piechart".
#' @return A plotly widget.
#' @examples
#' D <- data_gbd2021_cod() # cod data
#' cod <- D[D$region == "Romania" & D$sex == "both" & D$period == 2021, ]
#' plot_cod(cod)
#' @export
plot_cod <- function(cod, perc = FALSE, type = "barplot") {

  check_null(cod, "cod data")

  region = period = sex = cause_name <- NULL
  deaths = Deaths = COD <- NULL

  # Barplot: delegate to the native plotly version (same data prep as the
  # ggplot version this function replaced)
  if (type == "barplot") {
    p <- plotly_cod(cod, perc = perc)

  } else if (type == "piechart") {
    # Native plotly pie chart (mirror of the old coord_polar barplot)
    dt <- cod %>%
      group_by(region, period, sex, cause_name) %>%
      summarise(Deaths = sum(deaths)) %>%
      rename(COD = cause_name) %>%
      ungroup()

    if (perc) {
      dt <- dt %>%
        group_by(region, sex) %>%
        mutate(Deaths = Deaths / sum(Deaths) * 100) %>%
        ungroup()
    }

    # Sort by COD alphabetically so the positional marker colours
    # align with epidemiology_palette() (pie charts don't match by name).
    dt <- dt %>% arrange(as.character(COD))

    p <- plotly::plot_ly(
      data = dt,
      labels = ~COD,
      values = ~Deaths,
      type = "pie",
      textinfo = "label+percent",
      marker = list(
        colors = unname(epidemiology_palette()[as.character(dt$COD)]),
        line = list(color = "white", width = 1)
      )
    )

  } else {
    stop("type must be one of 'barplot' or 'piechart'")
  }

  return(p)
}

# ----------------------------------------------------------------------------
# Figure 4.

#' Plot a decomposition object
#'
#' @param object An object of class \code{decompose}.
#' @param by The dimensions on which to build the plot.
#' One of "both", "age" or "cod".
#' @inheritParams plot_cod
#' @seealso
#' \code{\link{decompose_by_cod}}
#' \code{\link{decompose_by_age}}
#' @return A plotly widget.
#' @examples
#' # See example in the ?decompose_by_cod or ?decompose_by_age help pages
#' @export
plot_decompose <- function(object,
                           perc = FALSE,
                           by = "both") {

  check_null(object, "decomposition data")

  # Delegate to plotly_decompose, which builds the decomposition figure
  p <- plotly_decompose(object, perc = perc, by = by)

  return(p)
}


# ----------------------------------------------------------------------------
# Native plotly versions of Figures 2-4.
#
# plotly_change(), plotly_cod() and plotly_decompose() all build the plotly
# objects directly with plot_ly() - no ggplot2 involved. plotly_decompose()
# rebuilds the v1.0.6 ggplot decomposition chart natively: one bar trace per
# cause stacked with barmode = "relative". 
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

  # y axis order follows epidemiology_palette() so COD categories appear
  # in their semantic-group order (cardiovascular, cancers, respiratory, ...).
  # Filter to names actually present in the data (palette may include
  # fine-grained SDG causes not in the current COD view).
  pal_order <- names(epidemiology_palette())
  dt$COD <- factor(dt$COD, levels = intersect(pal_order, unique(dt$COD)))

  xbr  <- pretty(c(0, max(dt$Deaths)), n = 5)
  xtic <- label_number_si(accuracy = 1)(xbr)

  one_plot <- function(sub) {
    plotly::plot_ly(
      data = sub,
      x = ~Deaths,
      y = ~COD,
      color = ~COD,
      colors = epidemiology_palette(),
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

    # Dynamic panel labels from the split names (country names or sexes)
    nms  <- names(spl)
    npan <- length(nms)
    panel_labels <- lapply(seq_len(npan), function(i) {
      list(
        x          = (i - 0.5) / npan,
        y          = 1.0,
        text       = nms[i],
        xref       = "paper",
        yref       = "paper",
        xanchor    = "center",
        yanchor    = "bottom",
        showarrow  = FALSE,
        font       = list(size = 13)
      )
    })

    p   <- p %>%
      plotly::layout(
        annotations = panel_labels,
        margin      = list(t = 50),
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
          ticksuffix = "\u00A0\u00A0"
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
          ticksuffix = "\u00A0\u00A0"
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

  # ---- Native plotly bars (no ggplot2) ------------------------------------
  # Figure 4 is built one bar trace per cause with barmode = "relative",
  # which stacks positive segments up (or right) from zero and negative
  # segments down (or left) from zero.
  cod_present <- function(cc) {
    rev(intersect(levels(d$COD), unique(as.character(cc))))
  }

  if (is.null(xlab)) {
    xlab <- if (by == "cod") "Causes of Death\nDecomposition"
            else "Age Group\n(Years)"
  }

  # Tooltip fields: "fill" is the cause, which native plotly renders as the
  # trace name; the remaining ttip fields pass through as hoverinfo, with the
  # trace name appended so the tooltip shows the cause on top of the value.
  # by = "cod" draws horizontal bars, so the value axis is x there (y is the
  # cause category): the "y" field maps onto "x" so the tooltip shows the
  # value, not the (already named) cause row.
  hi_flds <- setdiff(ttip, "fill")
  if (by == "cod") hi_flds[hi_flds == "y"] <- "x"
  hi <- paste(hi_flds, collapse = "+")
  if (by != "age" && "fill" %in% ttip) hi <- paste(hi, "name", sep = "+")
  if (hi == "") hi <- NULL

  d_pos <- d[d$`Change in LE` > 0, ]
  d_neg <- d[d$`Change in LE` < 0, ]

  rng  <- if (nrow(d)) range(d$`Change in LE`) else c(-1, 1)
  vbr  <- pretty(rng, n = 6)
  vtic <- label_number_si(accuracy = 0.01)(vbr)

  if (by == "cod") {
    # Horizontal bars, one per cause: each cause sits on its own category of
    # the y axis, so the axis ticks, tick labels and horizontal gridlines span
    # the full plot height (drawn all at y = sex, the constant "both", they
    # collapsed into a band around the middle of the panel). The signed
    # per-cause total sits on the value axis (x) with SI tick labels and a zero
    # line instead of a geom_vline() scatter trace. Bar width 0.5 as in v1.0.6.
    p <- plotly::plot_ly()
    for (cod in cod_present(d_pos$COD)) {
      tmp <- d_pos[d_pos$COD == cod, ]
      p <- plotly::add_bars(
        p, data = tmp,
        x = ~`Change in LE`, y = ~COD,
        name = cod, orientation = "h", width = 0.5,
        marker = list(color = unname(epidemiology_palette()[cod])),
        hoverinfo = hi)
    }
    for (cod in cod_present(d_neg$COD)) {
      tmp <- d_neg[d_neg$COD == cod, ]
      p <- plotly::add_bars(
        p, data = tmp,
        x = ~`Change in LE`, y = ~COD,
        name = cod, orientation = "h", width = 0.5, showlegend = FALSE,
        marker = list(color = unname(epidemiology_palette()[cod])),
        hoverinfo = hi)
    }
    p <- plotly::layout(
      p,
      barmode = "relative",
      showlegend = FALSE,
      xaxis = list(
        title          = xlab,
        titlefont      = list(size = 14),
        tickfont       = list(size = 11),
        tickvals       = vbr,
        ticktext       = vtic,
        zeroline       = TRUE,
        zerolinewidth  = 1,
        zerolinecolor  = "black"
      ),
      yaxis = list(
        title           = "",
        titlefont       = list(size = 14),
        tickfont        = list(size = 11),
        categoryorder   = "array",
        categoryarray   = levels(d$COD),
        ticks           = "outside"
      )
    )

  } else {
    # Vertical stacked bar: one trace per cause (by = "both") or a single
    # grey trace (by = "age", which has no cause column). The value axis (y)
    # carries the SI tick labels and a zero line instead of a geom_hline()
    # scatter trace.
    p <- plotly::plot_ly()
    if (by == "both") {
      for (cod in cod_present(d_pos$COD)) {
        tmp <- d_pos[d_pos$COD == cod, ]
        p <- plotly::add_bars(
          p, data = tmp,
          x = ~`Age Interval`, y = ~`Change in LE`,
          name = cod,
          marker = list(color = unname(epidemiology_palette()[cod])),
          hoverinfo = hi)
      }
      for (cod in cod_present(d_neg$COD)) {
        tmp <- d_neg[d_neg$COD == cod, ]
        p <- plotly::add_bars(
          p, data = tmp,
          x = ~`Age Interval`, y = ~`Change in LE`,
          name = cod, showlegend = FALSE,
          marker = list(color = unname(epidemiology_palette()[cod])),
          hoverinfo = hi)
      }
    } else {
      # Age-only decomposition: the v1.0.6 ggplot default fill (grey35) as a
      # single trace.
      p <- plotly::plot_ly(
        data = d,
        x = ~`Age Interval`, y = ~`Change in LE`, type = "bar",
        marker = list(color = "#595959"),
        hoverinfo = hi)
    }
    p <- plotly::layout(
      p,
      barmode = "relative",
      showlegend = FALSE,
      bargap = 0.1,
      xaxis = list(
        title     = xlab,
        titlefont = list(size = 14),
        tickfont  = list(size = 11),
        tickangle = 45
      ),
      yaxis = list(
        title          = ylab,
        titlefont      = list(size = 14),
        tickfont       = list(size = 11),
        tickvals       = vbr,
        ticktext       = vtic,
        zeroline       = TRUE,
        zerolinewidth  = 1,
        zerolinecolor  = "black"
      )
    )
  }

  return(p)
}


# ----------------------------------------------------------------------------
# Extras

#' Epidemiology cause-of-death colour palette
#'
#' Returns a named vector mapping cause-of-death categories to
#' semantically grouped colours (cardiovascular greens, cancers purples,
#' respiratory blues, infectious blacks/grays, etc.). The named palette is
#' matched by name in \pkg{plotly} \code{color}, ensuring that the same COD
#' always receives the same colour regardless of factor-level reordering.
#' @keywords internal
epidemiology_palette <- function() {
  c(
    # --- GROUP 1: CARDIOVASCULAR (The Vivid Lime-to-Green Anchor) ---
    "Cardiovascular Diseases"                     = "#00FF00", # Blinding Lime Green
    "Ischemic Heart Disease"                      = "#00CC00", # Rich Vivid Green
    "Other Cardiovascular"                        = "#A9DFBF", # Soft Mint (Fading out)
    
    # --- GROUP 2: STROKE (The Electric Purple-Blue Anchor) ---
    "Stroke"                                      = "#651FFF", # Deep Electric Violet
    
    # --- GROUP 3: NEOPLASMS / CANCERS (The Oncology Purple Anchor) ---
    "Neoplasms"                                   = "#311B92", # Midnight Royal Purple
    "Lung Cancer"                                 = "#4A148C", # Dark Amethyst
    "Colon and Rectum Cancer"                     = "#8E24AA", # Medium Magenta-Purple
    "Other Neoplasms"                             = "#E1BEE7", # Soft Lavender (Fading out)
    
    # --- GROUP 4: RESPIRATORY DISEASES (The Electric Cyan Anchor) ---
    "Chronic Respiratory diseases"                = "#00FFFF", # Blinding Cyan
    "COVID-19"                                    = "#00BFFF", # Deep Electric Blue
    "Respiratory Infections (excl. COVID)"        = "#005F9E", # Cobalt Blue
    "Respiratory Infections (excl. Tuberculosis)" = "#002F6C", # Deep Dark Navy
    
    # --- GROUP 5: COMMUNICABLE / INFECTIOUS (The Pitch Black Core) ---
    "HIV/ AIDS / STD"                             = "#111111", # Absolute Pitch Black
    "Infections (excl. Respiratory)"              = "#333333", # Dark Charcoal
    "Enteric Infections"                          = "#555555", # Medium Slate Gray
    "Malaria"                                     = "#777777", # Cool Steel Gray
    "Tuberculosis"                                = "#999999", # Muted Silver
    "Neglected Tropical Diseases (excl. Malaria)" = "#CCCCCC", # Pale Gray (Fading out)
    "Other Communicable"                          = "#EEEEEE", # Ghost White (Fading out)
    
    # --- GROUP 6: METABOLIC & ORGAN DISEASES (The Hot Neon Pink Anchor) ---
    # Association: Endocrine systems, insulin spikes, and high-intensity magenta tracking.
    "Diabetes and Kidney Diseases"                = "#FF007F", # Blinding Hot Pink
    "Diabetes mellitus"                           = "#FF409F", # Neon Rose
    "Kidney disease (excl. Diabetes)"             = "#FF80BF", # Bubblegum Pink
    "Digestive Diseases"                          = "#FFB3D9", # Cotton Candy Pink (Fading out)
    
    # --- GROUP 7: NEUROLOGICAL BUFFER (The Blinding Neon Yellow) ---
    # Association: Electrical brain mapping. Sits as a strong buffer block.
    "Neurological Disorders"                      = "#FFEA00", # Electric Neon Yellow
    
    # --- GROUP 8: MATERNAL & NEONATAL (The Vivid Ruby Red/Coral Anchor) ---
    # Association: Highly distinct from the pinks. Vitality, blood, childbirth, and early development.
    "Maternal and Neonatal"                       = "#D50000", # Vivid Ruby Red
    "Maternal disorders"                          = "#FF1744", # Bright Torch Red
    "Neonatal disorders"                          = "#FF8A80", # Soft Coral Pastel (Fading out)
    
    # --- GROUP 9: INJURIES, VIOLENCE & CATCH-ALL (Trauma Orange & Earth Tones) ---
    "Injuries"                                    = "#FF5722", # High-Contrast Safety Orange
    "Injuries (excl. Poisonings)"                 = "#E64A19", # Dark Trauma Rust
    "Transport Injuries"                          = "#FFA726", # Warning Amber
    "Poisonings"                                  = "#FFCC80", # Pale Toxic Apricot
    "Exposure to forces of nature"                = "#FFF9C4", # Pale Desert Sand (Fading out)
    "Self-Harm and Violence"                      = "#795548", # Muted Soil Brown
    "Self-harm"                                   = "#A1887F", # Light Earth Gray
    "Interpersonal Violence"                      = "#D7CCC8", # Fading Warm Stone
    "Other Non-Communicable"                      = "#B0BEC5"  # Cool Concrete Gray (Fading out)
  )
}




#' @keywords internal
check_null <- function(data, data_name = "data") {
  if (is.null(data)) {
    stop(paste(data_name, "is NULL - cannot proceed with plotting"))
  }
  return(TRUE)
}


