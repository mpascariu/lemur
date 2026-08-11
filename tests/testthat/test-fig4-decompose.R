# ------------------------------------------------- #
# Regression test: figure 4 (decomposition) results
# and rendering
#
# Pins the native-plotly encoding of the decomposition
# figure (no ggplot2), which reproduces the v1.0.6
# geometry with plotly::plot_ly() + add_bars() and
# barmode = "relative":
#
#   1. RESULT -- the grand sum of the cause-age
#      decomposition must equal the life-expectancy
#      gap between the two populations.
#
#   2. STACKING -- relative barmode stacks positive
#      segments up from zero and negative segments
#      down from zero, with the FIRST trace added
#      adjacent to zero. plotly_decompose() therefore
#      adds the bar traces in REVERSE factor order so
#      the last cause sits next to zero and the first
#      cause (COVID-19) at the top of the positive
#      stack and at the bottom of the negative stack --
#      the v1.0.6 position_stack geometry. Every bar
#      carries its raw signed value with base = NULL;
#      the stacks are assembled by plotly at render
#      time.
#
#   3. COLOURS -- cause colours are the raw hex values
#      from epidemiology_palette() matched by cause
#      name; the same COD always gets the same colour
#      regardless of factor-level position.
#
# Also pins the per-cause / per-age bar structure for
# the by = "cod" and by = "age" variants.
# ------------------------------------------------- #

dec <- decompose_by_cod(L_romania, L_mexico, D_romania, D_mexico)

bar_traces <- function(built) {
  built$x$data[vapply(built$x$data, function(t) identical(t$type, "bar"), logical(1))]
}
non_bar_traces <- function(built) {
  built$x$data[!vapply(built$x$data, function(t) identical(t$type, "bar"), logical(1))]
}

# Canonical age-group labels, in the order plotly_decompose() assigns them to
# the Age Interval factor (position k of x.int maps to age_labels[k]).
age_labels <- c("0", "1", "2-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                "60-64", "65-69", "70-74", "75-79", "80-84", "85-89",
                "90-94", "95-99", "100-104", "105-109", "+110")

# Causes present in the positive / negative buckets, in the trace order used by
# plotly_decompose(): REVERSE factor order (the last cause sits adjacent to zero
# in the relative stack). Rounding to 4 decimals mirrors the round() the figure
# applies before splitting by sign, so causes whose rounded contribution is 0
# stay out of both buckets.
cod_present <- function(cc) {
  rev(intersect(levels(dec$cause_name), unique(as.character(cc))))
}

dec4 <- dec
dec4$decomposition <- round(dec4$decomposition, 4)
pos_causes <- cod_present(dec4$cause_name[dec4$decomposition > 0])
neg_causes <- cod_present(dec4$cause_name[dec4$decomposition < 0])

test_that("decomposition result sums to the life-expectancy gap", {
  expect_equal(nrow(dec), 450)                      # 18 causes x 25 age groups
  expect_equal(nlevels(dec$cause_name), 18)
  expect_equal(sum(dec$decomposition),
               L_mexico$ex[1] - L_romania$ex[1],
               tolerance = 1e-9)
})

test_that("figure 4 (by='both') stacks in reverse factor order, v1.0.6 style", {
  g <- plotly::plotly_build(suppressWarnings(plotly_decompose(dec, by = "both")))
  bars <- bar_traces(g)

  # one bar trace per present cause: positive bucket first, negative bucket
  # second, each in reverse factor order
  expect_equal(length(bars), length(pos_causes) + length(neg_causes))
  nms <- vapply(bars, function(t) t$name, "")
  expect_identical(nms, c(pos_causes, neg_causes))

  # v1.0.6 geometry: the LAST trace of each bucket is the FIRST factor level
  # (COVID-19) -- top-most segment of the positive stack and bottom-most
  # segment of the negative stack (relative stacking puts the first-added trace
  # adjacent to zero, so reverse factor order restores the ggplot geometry)
  expect_identical(neg_causes[length(neg_causes)], levels(dec$cause_name)[1])
  expect_identical(tail(nms, 1), levels(dec$cause_name)[1])

  # every trace is a vertical bar with its raw signed value and base = NULL:
  # the stack geometry is assembled by plotly at render time
  orient <- vapply(bars, function(t) if (is.null(t$orientation)) "v" else t$orientation, "")
  expect_true(all(orient == "v"))
  expect_true(all(vapply(bars, function(t) is.null(t$base), logical(1))))

  # positive traces carry only y >= 0, negative traces only y <= 0
  pos_idx <- seq_along(pos_causes)
  neg_idx <- seq.int(length(pos_causes) + 1, length(bars))
  expect_true(all(vapply(bars[pos_idx], function(t) all(as.numeric(t$y) >= 0), logical(1))))
  expect_true(all(vapply(bars[neg_idx], function(t) all(as.numeric(t$y) <= 0), logical(1))))

  # reconstruct the full cause x age grid from the traces and compare it with
  # the rounded decomposition: the bars must carry exactly the decomposition
  # data, age-for-age and cause-for-cause
  trace_long <- do.call(rbind, lapply(bars, function(t) {
    data.frame(cause = t$name,
               age   = match(as.character(t$x), age_labels),
               y     = as.numeric(t$y))
  }))
  dec_long <- data.frame(cause = as.character(dec$cause_name),
                         age   = as.integer(dec$x.int),
                         y     = round(dec$decomposition, 4))
  dec_long <- dec_long[dec_long$y != 0, ]
  agg_t <- aggregate(y ~ cause + age, trace_long, sum)
  agg_d <- aggregate(y ~ cause + age, dec_long, sum)
  agg_t <- agg_t[order(agg_t$cause, agg_t$age), ]
  agg_d <- agg_d[order(agg_d$cause, agg_d$age), ]
  row.names(agg_t) <- NULL
  row.names(agg_d) <- NULL
  expect_equal(agg_t, agg_d, tolerance = 1e-6)

  # colours are the raw palette hex values matched by cause name
  cols <- vapply(bars, function(t) t$marker$color, "")
  expect_identical(cols, unname(epidemiology_palette()[nms]))

  # hoverinfo content: default ttip c("fill", "y") -> "y+name"
  hi <- vapply(bars, function(t) unique(as.character(t$hoverinfo)), "")
  expect_true(all(hi == "y+name"))

  # layout: relative stacking, grey bars 0.9 wide -> bargap 0.1, no zero-line
  # scatter trace (replaced by the yaxis zeroline), SI tick labels on the value
  # axis
  expect_equal(length(non_bar_traces(g)), 0)
  expect_identical(g$x$layout$barmode, "relative")
  expect_equal(g$x$layout$bargap, 0.1)
  expect_identical(g$x$layout$xaxis$type, "category")
  expect_equal(g$x$layout$xaxis$tickangle, 45)
  expect_true(isTRUE(g$x$layout$yaxis$zeroline))
  expect_identical(g$x$layout$yaxis$ticktext,
                   unname(label_number_si(accuracy = 0.01)(g$x$layout$yaxis$tickvals)))
})

test_that("figure 4 (by='cod') shows one horizontal bar per cause", {
  g <- plotly::plotly_build(suppressWarnings(plotly_decompose(dec, by = "cod")))
  bars <- bar_traces(g)

  # one horizontal trace per cause: positive causes first, then negative, each
  # in reverse factor order
  sums <- round(tapply(dec$decomposition, dec$cause_name, sum), 4)
  pos_s <- cod_present(names(sums)[sums > 0])
  neg_s <- cod_present(names(sums)[sums < 0])
  expect_equal(length(bars), length(pos_s) + length(neg_s))
  nms <- vapply(bars, function(t) t$name, "")
  expect_identical(nms, c(pos_s, neg_s))

  orient <- vapply(bars, function(t) if (is.null(t$orientation)) "v" else t$orientation, "")
  expect_true(all(orient == "h"))

  # fixed bar width 0.5 (geom_bar(width = 0.5)); the value axis carries the
  # signed per-cause total as the bar's x position
  widths <- vapply(bars, function(t) as.numeric(t$width)[1], numeric(1))
  expect_equal(widths, rep(0.5, length(bars)))
  xs <- vapply(bars, function(t) as.numeric(t$x)[1], numeric(1))
  # as.numeric() also drops the array dim that tapply() leaves behind
  expect_equal(xs, as.numeric(unname(sums[nms])))

  # no zero-line scatter trace; the zero line is the xaxis zeroline and the SI
  # tick labels sit on the value axis
  expect_equal(length(non_bar_traces(g)), 0)
  expect_true(isTRUE(g$x$layout$xaxis$zeroline))
  expect_identical(g$x$layout$barmode, "relative")
  expect_identical(g$x$layout$xaxis$ticktext,
                   unname(label_number_si(accuracy = 0.01)(g$x$layout$xaxis$tickvals)))

  # regression: the y axis must span the full plot height. Previously every
  # trace was drawn at y = the constant sex ("both"), so the category axis held
  # a single category and plotly centred its one label/tick/gridline in a band
  # around the middle of the panel. Now each cause sits on its OWN category, so
  # the axis carries all 18 cause names with a tick and gridline per row.
  expect_identical(vapply(bars, function(t) as.character(t$y)[1], ""), nms)
  expect_identical(g$x$layout$yaxis$type, "category")
  expect_identical(g$x$layout$yaxis$categoryorder, "array")
  expect_identical(g$x$layout$yaxis$categoryarray, levels(dec$cause_name))
  expect_false(isFALSE(g$x$layout$yaxis$showticklabels))  # cause names shown

  # horizontal bars put the value axis on x: the tooltip shows the value
  # ("x") with the trace name, not the (already named) cause row ("y")
  hi <- vapply(bars, function(t) unique(as.character(t$hoverinfo)), "")
  expect_true(all(hi == "x+name"))
})

test_that("figure 4 (by='age') shows one grey bar trace per age group", {
  g <- plotly::plotly_build(suppressWarnings(plotly_decompose(dec, by = "age")))
  bars <- bar_traces(g)

  # a single trace with no name (age has no cause dimension), grey bars
  expect_equal(length(bars), 1)
  expect_true(is.null(bars[[1]]$name))
  expect_identical(bars[[1]]$marker$color, "#595959")

  # the bar values are the signed per-age sums, with base = NULL (relative
  # stacking assembles the segments at render time)
  asum <- round(tapply(dec$decomposition, dec$x, sum), 4)
  expect_equal(as.numeric(bars[[1]]$y), as.numeric(asum), tolerance = 1e-9)
  expect_true(is.null(bars[[1]]$base))

  # category x axis carrying the grouped age labels, not a linear 1:25 axis
  expect_identical(as.character(bars[[1]]$x), age_labels)
  expect_identical(g$x$layout$xaxis$type, "category")
  expect_equal(g$x$layout$xaxis$tickangle, 45)
  expect_equal(g$x$layout$bargap, 0.1)

  # zero line via the yaxis zeroline (no scatter trace), SI tick labels
  expect_equal(length(non_bar_traces(g)), 0)
  expect_true(isTRUE(g$x$layout$yaxis$zeroline))
  expect_identical(g$x$layout$yaxis$ticktext,
                   unname(label_number_si(accuracy = 0.01)(g$x$layout$yaxis$tickvals)))
})

test_that("figure 4 colours stay stable when a cause level is absent", {
  # Re-level the decomposition so an unused cause sits between two real ones.
  # A present cause at a high factor position must keep its named-palette
  # colour, i.e. colours map by cause NAME, not by the compressed position
  # among present causes (named epidemiology_palette(), drop = FALSE behaviour).
  levs <- levels(dec$cause_name)
  ghost <- c(levs[1:17], "Ghost Cause", levs[18])
  dec2 <- dec
  dec2$cause_name <- factor(as.character(dec2$cause_name), levels = ghost)

  g <- plotly::plotly_build(suppressWarnings(plotly_decompose(dec2, by = "both")))
  bars <- bar_traces(g)

  # the absent level produces no bar trace; only the present causes are drawn,
  # in the same reverse-factor-order sequence as before
  expect_equal(length(bars), length(pos_causes) + length(neg_causes))
  names2 <- vapply(bars, function(t) t$name, "")
  expect_false("Ghost Cause" %in% names2)

  # named palette: each cause gets its colour from epidemiology_palette() by
  # name, regardless of where the ghost cause shifts its factor-level position.
  # The last factor level appears in both buckets (first trace of each), so the
  # colour must be identical for every occurrence.
  cols2 <- vapply(bars, function(t) t$marker$color, "")
  expect_identical(cols2, unname(epidemiology_palette()[names2]))
  expect_identical(unique(cols2[names2 == levs[18]]),
                   unname(epidemiology_palette()[levs[18]]))
})
