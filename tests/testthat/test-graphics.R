# ------------------------------------------------- #
# Unit tests: graphics functions
#
# Covers R/fun-figures.R -- the plot_* (ggplot) and
# plotly_* (plotly) functions, plus the small helpers
# glasbey(), check_null() and plot_theme().
#
# These are structural / smoke tests: we assert the
# returned object's class and that valid inputs run
# without error. Exact pixel content is not tested.
# ------------------------------------------------- #

test_that("glasbey returns a fixed colour palette", {
  cols <- glasbey()
  expect_equal(length(cols), 30)
  expect_true(all(grepl("^#", cols)))
  expect_equal(unique(cols), cols)
})

test_that("check_null errors on NULL and passes otherwise", {
  expect_error(check_null(NULL, "map data"), "map data is NULL")
  expect_true(check_null(data.frame(x = 1), "data"))
})

test_that("plot_theme returns a ggplot theme", {
  expect_s3_class(plot_theme(), "theme")
})

test_that("plot_change returns a ggplot and handles percentages", {
  p <- suppressWarnings(plot_change(L_romania, L_mexico))
  expect_s3_class(p, "ggplot")

  p_perc <- suppressWarnings(plot_change(L_romania, L_mexico, perc = TRUE))
  expect_s3_class(p_perc, "ggplot")

  p_age <- suppressWarnings(plot_change(L_romania, L_mexico, age = c(0, 50, 110)))
  expect_s3_class(p_age, "ggplot")

  # NULL input is caught by check_null
  expect_error(plot_change(NULL, L_mexico), "Life Table 1 is NULL")
})

test_that("plot_cod returns a ggplot for both chart types", {
  p_bar <- suppressWarnings(plot_cod(D_romania))
  expect_s3_class(p_bar, "ggplot")

  p_pie <- suppressWarnings(plot_cod(D_romania, type = "piechart"))
  expect_s3_class(p_pie, "ggplot")

  p_perc <- suppressWarnings(plot_cod(D_romania, perc = TRUE))
  expect_s3_class(p_perc, "ggplot")
})

test_that("plot_decompose returns a ggplot for every 'by' dimension", {
  dec_age <- decompose_by_age(L_romania, L_mexico)
  dec_cod <- decompose_by_cod(L_romania, L_mexico, D_romania, D_mexico)

  # age-only decomposition (no cause_name) forces by = "age"
  p_age <- suppressWarnings(plot_decompose(dec_age))
  expect_s3_class(p_age, "ggplot")

  p_both <- suppressWarnings(plot_decompose(dec_cod, by = "both"))
  expect_s3_class(p_both, "ggplot")

  p_cod <- suppressWarnings(plot_decompose(dec_cod, by = "cod"))
  expect_s3_class(p_cod, "ggplot")

  p_perc <- suppressWarnings(plot_decompose(dec_cod, perc = TRUE))
  expect_s3_class(p_perc, "ggplot")
})

test_that("plotly_change returns a plotly widget", {
  p <- suppressWarnings(plotly_change(L_romania, L_mexico))
  expect_s3_class(p, "plotly")

  p_perc <- suppressWarnings(plotly_change(L_romania, L_mexico, perc = TRUE))
  expect_s3_class(p_perc, "plotly")
})

test_that("plotly_cod returns a plotly widget and facets comparison modes", {
  p_single <- suppressWarnings(plotly_cod(D_romania))
  expect_s3_class(p_single, "plotly")

  p_perc <- suppressWarnings(plotly_cod(D_romania, perc = TRUE))
  expect_s3_class(p_perc, "plotly")

  # comparison modes build a two-panel subplot
  cod_two <- bind_rows(D_romania, D_mexico)
  p_cntr <- suppressWarnings(plotly_cod(cod_two, mode = "mode_cntr"))
  expect_s3_class(p_cntr, "plotly")

  sex_two <- bind_rows(
    D_full[D_full$region == "Romania" & D_full$sex == "male" & D_full$period == 2021, ],
    D_full[D_full$region == "Romania" & D_full$sex == "female" & D_full$period == 2021, ]
  )
  p_sex <- suppressWarnings(plotly_cod(sex_two, mode = "mode_sex"))
  expect_s3_class(p_sex, "plotly")
})

test_that("plotly_decompose returns a plotly widget for every 'by' dimension", {
  dec_cod <- decompose_by_cod(L_romania, L_mexico, D_romania, D_mexico)

  p_both <- suppressWarnings(plotly_decompose(dec_cod, by = "both"))
  expect_s3_class(p_both, "plotly")

  p_cod <- suppressWarnings(plotly_decompose(dec_cod, by = "cod"))
  expect_s3_class(p_cod, "plotly")

  p_age <- suppressWarnings(plotly_decompose(dec_cod, by = "age"))
  expect_s3_class(p_age, "plotly")

  p_perc <- suppressWarnings(plotly_decompose(dec_cod, perc = TRUE))
  expect_s3_class(p_perc, "plotly")
})

test_that("plot_map returns a leaflet widget", {
  skip_on_cran()
  p <- suppressWarnings(plot_map(location = "Romania"))
  expect_s3_class(p, "leaflet")
})
