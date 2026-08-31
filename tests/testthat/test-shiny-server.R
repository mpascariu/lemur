# ------------------------------------------------- #
# Unit tests: Shiny server logic (pure functions)
#
# Covers R/app_server.R, R/app_captions.R,
# R/app_data_filters.R, R/fun-utils.R.
#
# The app_server reactive graph is driven by golem's
# getShinyOption() and a large input set, so we test the
# pure-function parts it delegates to rather than the
# full reactive pipeline (which would need testServer()
# plus a large mocked input set).
# ------------------------------------------------- #

test_that("app_server is a function", {
  expect_type(app_server, "closure")
})

test_that("table captions are produced for every comparison mode", {
  for (mode in c("mode_cod", "mode_cntr", "mode_sex", "mode_sdg", "mode_sdg2")) {
    caps <- generate_table_captions(mode, "Romania", "Mexico", 2021, "both", -50)
    expect_equal(length(caps), 6)
    expect_true(all(nzchar(caps)))
  }

  # country comparison mentions both regions in the decomposition caption
  caps <- generate_table_captions("mode_cntr", "Romania", "Mexico", 2021, "both", 0)
  expect_true(grepl("between Romania and Mexico", caps[5], fixed = TRUE))

  # single-region modes use one region in the caption
  caps <- generate_table_captions("mode_cod", "Romania", "Romania", 2021, "both", 0)
  expect_true(grepl("Romania", caps[1], fixed = TRUE))
})

test_that("fig2 captions build an xlab/ylab pair", {
  f2 <- generate_fig2_captions("mode_cod", "Romania", "Romania",
                               c(0, 50, 95), FALSE, -50, c("Stroke"),
                               L_romania, L_mexico)
  expect_equal(names(f2), c("xlab", "ylab"))
  expect_true(nzchar(f2$xlab))
  expect_equal(f2$ylab, "Age (years)")

  # percentage mode changes the wording of the xlab
  f2_perc <- generate_fig2_captions("mode_cod", "Romania", "Romania",
                                    c(0, 50, 95), TRUE, -50, c("Stroke"),
                                    L_romania, L_mexico)
  expect_true(grepl("Relative difference", f2_perc$xlab, fixed = TRUE))
})

test_that("fig3 captions switch between percentage and counts", {
  expect_true(grepl("%", generate_fig3_captions(TRUE), fixed = TRUE))
  expect_true(grepl("Number of Deaths", generate_fig3_captions(FALSE), fixed = TRUE))
})

test_that("fig4 captions report the right tooltip for each dimension", {
  expect_equal(generate_fig4_captions(FALSE, "cod")$ttip, c("fill", "x"))
  expect_equal(generate_fig4_captions(FALSE, "age")$ttip, c("fill", "y"))
  expect_equal(generate_fig4_captions(TRUE, "cod")$ttip, c("fill", "x"))
  expect_equal(generate_fig4_captions(TRUE, "age")$ttip, c("fill", "y"))
  expect_true(all(c("xlab", "ylab", "ttip") %in% names(generate_fig4_captions(FALSE, "cod"))))
})

test_that("generate_figure_captions orchestrates fig2/3/4", {
  ff <- generate_figure_captions("mode_cod", "Romania", "Romania",
                                 c(0, 50, 95), FALSE, -50, c("Stroke"),
                                 L_romania, L_mexico, "age")
  expect_equal(names(ff), c("fig2", "fig3", "fig4"))
  expect_equal(names(ff$fig2), c("xlab", "ylab"))
  expect_true(is.character(ff$fig3))
  expect_true(all(c("xlab", "ylab", "ttip") %in% names(ff$fig4)))
})

test_that("create_db_pool returns NULL in local mode", {
  expect_null(create_db_pool(run_db = FALSE))
})

test_that("db_setting falls back to the default and reads the environment", {
  expect_equal(db_setting("LEMUR_TEST_UNSET_VAR", "fallback"), "fallback")

  Sys.setenv(LEMUR_TEST_SET_VAR = "env-value")
  on.exit(Sys.unsetenv("LEMUR_TEST_SET_VAR"), add = TRUE)
  expect_equal(db_setting("LEMUR_TEST_SET_VAR", "fallback"), "env-value")
})

test_that("format_datatable returns a DT widget with the caption", {
  dt <- format_datatable(head(L_romania, 3), "Test caption")
  expect_s3_class(dt, "datatables")
  expect_true(any(grepl("Test caption", as.character(dt), fixed = TRUE)))
})
