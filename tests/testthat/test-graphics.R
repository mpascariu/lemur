# ------------------------------------------------- #
# Unit tests: graphics functions
#
# Covers R/fun-figures.R -- the exported plot_* and
# internal plotly_* functions, plus the small helpers
# check_null() and epidemiology_palette().
#
# These are structural / smoke tests: we assert the
# returned object's class and that valid inputs run
# without error. Exact pixel content is not tested.
# ------------------------------------------------- #

test_that("epidemiology_palette returns a named colour palette", {
  pal <- epidemiology_palette()
  expect_true(length(pal) >= 18)                   # at minimum the 18 COD categories
  expect_true(all(grepl("^#", pal)))
  expect_equal(length(unique(pal)), length(pal))   # no duplicate colours
  expect_named(pal)
  # every original COD name is present in the palette
  cod_names <- c(
    "Chronic Respiratory diseases",
    "Colon and Rectum Cancer",
    "COVID-19",
    "Diabetes and Kidney Diseases",
    "Digestive Diseases",
    "HIV/ AIDS / STD",
    "Infections (excl. Respiratory)",
    "Injuries",
    "Ischemic Heart Disease",
    "Lung Cancer",
    "Maternal and Neonatal",
    "Neurological Disorders",
    "Other Cardiovascular",
    "Other Neoplasms",
    "Other Non-Communicable",
    "Respiratory Infections (excl. COVID)",
    "Self-Harm and Violence",
    "Stroke"
  )
  expect_true(all(cod_names %in% names(pal)))
  # every palette entry is a valid hex colour with no duplicates
  expect_equal(length(unique(pal)), length(pal))
})

test_that("check_null errors on NULL and passes otherwise", {
  expect_error(check_null(NULL, "map data"), "map data is NULL")
  expect_true(check_null(data.frame(x = 1), "data"))
})

test_that("plot_change returns a plotly widget and handles percentages", {
  p <- suppressWarnings(plot_change(L_romania, L_mexico))
  expect_s3_class(p, "plotly")

  p_perc <- suppressWarnings(plot_change(L_romania, L_mexico, perc = TRUE))
  expect_s3_class(p_perc, "plotly")

  p_age <- suppressWarnings(plot_change(L_romania, L_mexico, age = c(0, 50, 95)))
  expect_s3_class(p_age, "plotly")

  # NULL input is caught by check_null
  expect_error(plot_change(NULL, L_mexico), "Life Table 1 is NULL")
})

test_that("plot_cod returns a plotly widget for both chart types", {
  p_bar <- suppressWarnings(plot_cod(D_romania))
  expect_s3_class(p_bar, "plotly")

  p_pie <- suppressWarnings(plot_cod(D_romania, type = "piechart"))
  expect_s3_class(p_pie, "plotly")

  p_perc <- suppressWarnings(plot_cod(D_romania, perc = TRUE))
  expect_s3_class(p_perc, "plotly")

  # invalid chart type errors out instead of returning an unbound object
  expect_error(plot_cod(D_romania, type = "nonsense"), "barplot")
})

test_that("plot_decompose returns a plotly widget for every 'by' dimension", {
  dec_age <- decompose_by_age(L_romania, L_mexico)
  dec_cod <- decompose_by_cod(L_romania, L_mexico, D_romania, D_mexico)

  # age-only decomposition (no cause_name) forces by = "age"
  p_age <- suppressWarnings(plot_decompose(dec_age))
  expect_s3_class(p_age, "plotly")

  p_both <- suppressWarnings(plot_decompose(dec_cod, by = "both"))
  expect_s3_class(p_both, "plotly")

  p_cod <- suppressWarnings(plot_decompose(dec_cod, by = "cod"))
  expect_s3_class(p_cod, "plotly")

  p_perc <- suppressWarnings(plot_decompose(dec_cod, perc = TRUE))
  expect_s3_class(p_perc, "plotly")
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
