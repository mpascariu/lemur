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
#      adjacent to zero. Cause order follows
#      epidemiology_palette() -- the same colour order
#      as figure 3 -- NOT the COD factor levels: the
#      positive bucket adds traces in REVERSE palette
#      order (the last palette cause sits adjacent to
#      zero, the first -- Chronic Respiratory diseases -- at
#      the top of the positive stack); the negative
#      bucket adds traces in FORWARD palette order so
#      the cause adjacent to zero is also the first
#      palette cause, and reading down from the zero
#      line follows figure 3. Every bar carries its raw
#      signed value with base = NULL; the stacks are
#      assembled by plotly at render time.
#
#   3. COLOURS -- cause colours are the raw hex values
#      from epidemiology_palette() matched by cause
#      name; the same COD always gets the same colour
#      regardless of factor-level position.
#
# Also pins the rendering of the by = "cod" variant -- a SINGLE horizontal
# stacked bar whose segments are the per-cause totals -- and of the
# by = "age" variant (one grey bar per age group).
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
                "90-94", "+95")

# Causes present in the positive / negative buckets, in the trace order used by
# plotly_decompose(): PALETTE order -- the same colour order as figure 3 (NOT
# the COD factor levels). The positive bucket adds traces in REVERSE palette
# order (the last palette cause sits adjacent to zero, the first palette cause
# -- Chronic Respiratory diseases -- at the top of the positive stack); the
# negative bucket adds traces in FORWARD palette order so the cause adjacent to
# zero is
# also the first palette cause. Rounding to 4 decimals mirrors the round() the
# figure applies before splitting by sign, so causes whose rounded contribution
# is 0 stay out of both buckets.
pal_order <- function(cc) {
  intersect(names(epidemiology_palette()), unique(as.character(cc)))
}

dec4 <- dec
dec4$decomposition <- round(dec4$decomposition, 4)
pos_causes <- rev(pal_order(dec4$cause_name[dec4$decomposition > 0]))
neg_causes <- pal_order(dec4$cause_name[dec4$decomposition < 0])

test_that("decomposition result sums to the life-expectancy gap", {
  expect_equal(nrow(dec), 396)                      # 18 causes x 22 age groups
  expect_equal(nlevels(dec$cause_name), 18)
  expect_equal(sum(dec$decomposition),
               L_mexico$ex[1] - L_romania$ex[1],
               tolerance = 1e-9)
})

test_that("figure 4 (by='both') stacks in palette order, figure 3 style", {
  g <- plotly::plotly_build(suppressWarnings(plotly_decompose(dec, by = "both")))
  bars <- bar_traces(g)

  # one bar trace per present cause: positive bucket first in reverse palette
  # order, negative bucket second in forward palette order
  expect_equal(length(bars), length(pos_causes) + length(neg_causes))
  nms <- vapply(bars, function(t) t$name, "")
  expect_identical(nms, c(pos_causes, neg_causes))

  # figure-3 geometry: reading either stack top-down follows
  # epidemiology_palette() -- Chronic Respiratory diseases (the first palette
  # cause present) is the top-most segment of the positive stack AND sits
  # adjacent to zero in the negative stack, and the bottom of the negative
  # stack is Maternal and Neonatal (the last palette cause present with a
  # negative contribution). (Relative stacking puts the first-added trace
  # adjacent to zero, so the positive bucket is added in reverse palette order
  # and the negative bucket in forward palette order.)
  expect_identical(tail(pos_causes, 1), "Chronic Respiratory diseases")
  expect_identical(neg_causes[1], "Chronic Respiratory diseases")
  expect_identical(tail(nms, 1), "Maternal and Neonatal")

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

  # y-axis range: from the sum of the negative contributions to the sum of the
  # positive contributions, using for each side the age that brings the largest
  # outcome (the most-negative per-age sum and the most-positive per-age sum),
  # so every stacked bar sits exactly between the two axis bounds
  neg_by_age <- tapply(pmin(dec4$decomposition, 0), dec4$x, sum)
  pos_by_age <- tapply(pmax(dec4$decomposition, 0), dec4$x, sum)
  expect_equal(g$x$layout$yaxis$range,
               c(min(neg_by_age), max(pos_by_age)))
})

test_that("figure 4 (by='cod') is a single horizontal stacked bar", {
  g <- plotly::plotly_build(suppressWarnings(plotly_decompose(dec, by = "cod")))
  bars <- bar_traces(g)

  # one horizontal trace per present cause: positive causes first in reverse
  # palette order, negative causes after in forward palette order (figure 3's
  # colour order) -- the same trace sequence as the vertical by = "both" stacks
  sums <- round(tapply(dec$decomposition, dec$cause_name, sum), 4)
  pos_s <- rev(pal_order(names(sums)[sums > 0]))
  neg_s <- pal_order(names(sums)[sums < 0])
  expect_equal(length(bars), length(pos_s) + length(neg_s))
  nms <- vapply(bars, function(t) t$name, "")
  expect_identical(nms, c(pos_s, neg_s))

  orient <- vapply(bars, function(t) if (is.null(t$orientation)) "v" else t$orientation, "")
  expect_true(all(orient == "h"))

  # EVERY trace is drawn at the SAME constant y category, so barmode =
  # "relative" assembles them all into ONE horizontal stacked bar: positive
  # segments extend right from zero, negative segments left
  ycats <- vapply(bars, function(t) as.character(t$y)[1], "")
  expect_identical(ycats, rep("Causes of Death", length(bars)))

  # bar width 0.8 (fraction of the single category band); the value axis
  # carries the signed per-cause total as the bar's x extent
  widths <- vapply(bars, function(t) as.numeric(t$width)[1], numeric(1))
  expect_equal(widths, rep(0.8, length(bars)))
  xs <- vapply(bars, function(t) as.numeric(t$x)[1], numeric(1))
  # as.numeric() also drops the array dim that tapply() leaves behind
  expect_equal(xs, as.numeric(unname(sums[nms])))

  # positive segments extend only right of zero, negative only left
  pos_idx <- seq_along(pos_s)
  neg_idx <- seq.int(length(pos_s) + 1, length(bars))
  expect_true(all(vapply(bars[pos_idx], function(t) all(as.numeric(t$x) >= 0), logical(1))))
  expect_true(all(vapply(bars[neg_idx], function(t) all(as.numeric(t$x) <= 0), logical(1))))

  # colours are the raw palette hex values matched by cause name
  cols <- vapply(bars, function(t) t$marker$color, "")
  expect_identical(cols, unname(epidemiology_palette()[nms]))

  # no zero-line scatter trace; the zero line is the xaxis zeroline and the SI
  # tick labels sit on the value axis
  expect_equal(length(non_bar_traces(g)), 0)
  expect_true(isTRUE(g$x$layout$xaxis$zeroline))
  expect_identical(g$x$layout$barmode, "relative")
  expect_identical(g$x$layout$xaxis$ticktext,
                   unname(label_number_si(accuracy = 0.01)(g$x$layout$xaxis$tickvals)))

  # the y axis holds exactly ONE category -- "Causes of Death" -- which is what
  # collapses every trace onto the single horizontal bar
  expect_identical(g$x$layout$yaxis$type, "category")
  expect_identical(as.character(g$x$layout$yaxis$categoryarray), "Causes of Death")

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

  # y-axis range: from the sum of the negative contributions to the sum of the
  # positive contributions, using for each side the age that brings the largest
  # outcome (with by = "age" each age holds a single net value, so the bounds
  # are the most-negative and most-positive per-age sums)
  expect_equal(g$x$layout$yaxis$range,
               c(min(pmin(asum, 0)), max(pmax(asum, 0))))
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
  # in the same palette-order sequence as before
  expect_equal(length(bars), length(pos_causes) + length(neg_causes))
  names2 <- vapply(bars, function(t) t$name, "")
  expect_false("Ghost Cause" %in% names2)

  # named palette: each cause gets its colour from epidemiology_palette() by
  # name, regardless of where the ghost cause shifts its factor-level position
  # (trace order and colour both follow the palette, never the factor levels).
  # Self-Harm and Violence appears in both buckets, so its colour must be
  # identical for every occurrence.
  cols2 <- vapply(bars, function(t) t$marker$color, "")
  expect_identical(cols2, unname(epidemiology_palette()[names2]))
  expect_identical(unique(cols2[names2 == levs[18]]),
                   unname(epidemiology_palette()[levs[18]]))
})
