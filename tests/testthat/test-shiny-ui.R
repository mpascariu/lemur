# ------------------------------------------------- #
# Unit tests: Shiny UI construction
#
# Covers R/app_ui.R, R/app_ui_dashboard.R, R/app_ui_datatab.R
#
# These call the UI-builder functions (which need no
# running session) and check the structure of the tag
# trees they return.
# ------------------------------------------------- #

test_that("app_ui returns a tag list", {
  expect_s3_class(app_ui(), "shiny.tag.list")
})

test_that("ui_tabs builds the six navigation panels", {
  tabs <- ui_tabs()
  expect_s3_class(tabs, "shiny.tag.list")

  html <- as.character(tabs)
  for (title in c("Dashboard", "Data", "Methods", "Sources", "About", "Contact")) {
    expect_true(grepl(title, html, fixed = TRUE))
  }
})

test_that("ui_dashbord includes the loading overlay", {
  dash <- ui_dashbord()
  expect_s3_class(dash, "shiny.tag")
  expect_true(grepl("lemur-loading", as.character(dash), fixed = TRUE))
})

test_that("golem_add_external_resources returns a head tag", {
  res <- suppressWarnings(golem_add_external_resources())
  expect_s3_class(res, "shiny.tag")
})

test_that("tab_md builds a navigation panel with markdown", {
  expect_s3_class(tab_md("Methods", "app/www/doc_methods.md"), "shiny.tag")
})

test_that("ui_datatab builds the data tab with table outputs", {
  dat <- ui_datatab()
  expect_s3_class(dat, "shiny.tag")

  html <- as.character(dat)
  for (out in c("lt_initial", "lt_final", "cod_initial", "cod_final",
                "decomposition_data", "reduction_matrix")) {
    expect_true(grepl(out, html, fixed = TRUE))
  }
})

test_that("chart containers are cards holding plotly/leaflet outputs", {
  expect_s3_class(chart_1(), "shiny.tag")
  expect_true(grepl("figure1", as.character(chart_1()), fixed = TRUE))
  expect_true(grepl("figure2", as.character(chart_2()), fixed = TRUE))
  expect_true(grepl("figure3", as.character(chart_3()), fixed = TRUE))
  expect_true(grepl("figure4", as.character(chart_4()), fixed = TRUE))
})

test_that("boxTitleInput2 returns a card header with its title", {
  box <- boxTitleInput2("My Title")
  expect_s3_class(box, "shiny.tag")
  expect_true(grepl("My Title", as.character(box), fixed = TRUE))
})

test_that("slider_input_ returns a slider with the given id", {
  sl <- slider_input_(inputId = "sdg_1", label = "Risk")
  expect_s3_class(sl, "shiny.tag")
  expect_true(grepl("sdg_1", as.character(sl), fixed = TRUE))
})

test_that("top, side and main panels build their regions", {
  expect_s3_class(top_panel(), "shiny.tag")
  expect_s3_class(side_panel(), "shiny.tag.list")
  expect_s3_class(main_panel(), "shiny.tag")

  # side panel hosts the region/year/cause inputs
  html <- as.character(side_panel())
  for (id in c("region1", "time_slider", "cod_change", "age_change", "cod_target")) {
    expect_true(grepl(id, html, fixed = TRUE))
  }
})

test_that("UI can be built via lemur:: without attaching the package", {
  # Regression: data_app_input is lazy data and R binds data/ objects to the
  # namespace only on attach, so lemur::run_app() in a fresh session (no
  # library(lemur)) failed with "object 'data_app_input' not found" while
  # building the UI. .onLoad() now binds it at namespace load; this subprocess
  # attaches nothing, so it exercises exactly that path.
  rscript <- file.path(R.home("bin"),
                       if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  # Run the probe from a script file rather than `Rscript -e`: system2() passes
  # arguments through the shell on unix (`sh -c`), and R does not shell-quote
  # parentheses, so `-e "invisible(lemur:::app_ui())"` died on macOS/linux with
  # "syntax error near unexpected token `('". A script file never reaches the
  # shell command line, so it is safe on every platform.
  probe <- tempfile(fileext = ".R")
  writeLines("invisible(lemur:::app_ui())", probe)
  on.exit(unlink(probe), add = TRUE)
  out <- suppressWarnings(system2(
    rscript,
    args = c("--vanilla", probe),
    stdout = TRUE, stderr = TRUE
  ))
  msg <- paste(out, collapse = "\n")
  if (grepl("no package called .?lemur.?", msg)) {
    skip("lemur not installed in a library visible to the subprocess")
  }
  st <- attr(out, "status")
  expect_true(is.null(st) || identical(st, 0L) || identical(st, 0),
              info = paste("subprocess output:", msg))
})
