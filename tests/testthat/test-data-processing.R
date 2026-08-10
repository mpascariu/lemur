# ------------------------------------------------- #
# Unit tests: data processing functions
#
# Covers R/fun-modify_LT.R, R/fun-decompose.R,
# R/app_data_processing.R, R/app_data_filters.R, R/fun-utils.R
#
# Fixtures are defined in setup.R. Tests use small slices
# (single region / single year) so the whole file runs fast.
# ------------------------------------------------- #

test_that("accessor functions return the expected data", {
  L <- data_gbd2021_lt()
  D <- data_gbd2021_cod()
  S <- data_gbd2021_sdg()

  expect_s3_class(L, "data.table")
  expect_s3_class(D, "data.table")
  expect_s3_class(S, "data.table")

  expect_equal(nrow(L), 148500)
  expect_equal(nrow(D), 2673000)
  expect_equal(nrow(S), 3118500)

  expect_true(all(c("x", "region", "sex", "period", "ex") %in% names(L)))
  expect_true(all(c("x", "region", "sex", "period", "cause_name", "deaths") %in% names(D)))
  expect_true(all(c("x", "region", "sex", "period", "cause_name", "deaths") %in% names(S)))
})

test_that("number() and label_number_si() format known values", {
  expect_equal(number(1234.567, accuracy = 0.01), "1 234.57")
  expect_equal(number(1234567, accuracy = 1), "1 234 567")
  expect_equal(number(0, accuracy = 1), "0")

  expect_equal(label_number_si(accuracy = 1)(1234), "1K")
  expect_equal(label_number_si(accuracy = 0.01)(1234), "1.23K")
  expect_equal(label_number_si()(2500000), "2M")
})

test_that("build_cod_matrix returns a proportions matrix", {
  M <- build_cod_matrix(D_romania)

  expect_true(is.matrix(M))
  expect_equal(dim(M), c(25, 18))
  expect_equal(rownames(M), as.character(sort(unique(D_romania$x))))
  expect_equal(colnames(M), as.character(unique(D_romania$cause_name)))
  # each age group's shares sum to 1
  expect_lt(max(abs(rowSums(M) - 1)), 1e-6)

  # a row with zero deaths is filled with a uniform share
  D_z <- data.table::copy(D_romania)
  D_z$deaths[D_z$x == 50] <- 0
  Mz <- build_cod_matrix(D_z)
  expect_equal(Mz["50", ], rep(1/18, 18), ignore_attr = TRUE)
})

test_that("modify_cod applies scalar, vector and matrix changes", {
  cod <- build_cod_matrix(D_romania)

  # scalar: -50% halves every cell
  expect_equal(modify_cod(cod, -50), cod * 0.5)

  # vector: column j scaled by (1 + j/100)
  v <- modify_cod(cod, 1:18)
  expect_equal(v[, 1], cod[, 1] * 1.01)
  expect_equal(v[, 18], cod[, 18] * 1.18)

  # matrix: element-wise
  m <- matrix(10, nrow = 25, ncol = 18)
  expect_equal(modify_cod(cod, m), cod * 1.1)

  # a 100% reduction is rejected
  expect_error(modify_cod(cod, -100), "100%")
  expect_error(modify_cod(cod, matrix(-100, 25, 18)), "100%")
})

test_that("build_reduction_matrix fills the selected ages and causes only", {
  R <- build_reduction_matrix(
    data        = D_romania,
    select_cod  = c("Stroke", "Ischemic Heart Disease"),
    select_x    = 45:75,
    cod_change  = -30
  )

  expect_equal(dim(R), c(25, 18))
  expect_equal(rownames(R), as.character(sort(unique(D_romania$x))))
  expect_equal(colnames(R), as.character(unique(D_romania$cause_name)))

  sel <- as.character(c(45, 50, 55, 60, 65, 70, 75))
  # selected ages x selected causes are set
  expect_equal(unique(R[sel, "Stroke"]), -30)
  expect_equal(unique(R[sel, "Ischemic Heart Disease"]), -30)
  # unselected ages are zero
  expect_equal(R[as.character(0), "Stroke"], 0)
  expect_equal(R[as.character(110), "Stroke"], 0)
  # unselected causes are zero
  expect_equal(R[as.character(50), "COVID-19"], 0)
})

test_that("modify_cod_table preserves structure and modifies deaths", {
  # data.table input (the accessor format)
  out_dt <- modify_cod_table(D_romania, cod_change = -50)
  expect_equal(nrow(out_dt), nrow(D_romania))
  expect_equal(names(out_dt), names(D_romania))
  expect_equal(out_dt$deaths, D_romania$deaths / 2)

  # tibble input (the app path)
  out_tb <- modify_cod_table(tibble::as_tibble(D_romania), cod_change = -50)
  expect_equal(out_tb$deaths, D_romania$deaths / 2)
  expect_equal(levels(out_tb$cause_name), levels(D_romania$cause_name))

  # zero change leaves deaths untouched
  out0 <- modify_cod_table(D_romania, cod_change = 0)
  expect_equal(out0$deaths, D_romania$deaths)
})

test_that("modify_life_table raises life expectancy on reduction and lowers on increase", {
  e0_0 <- modify_life_table(L_romania, D_romania, cod_change = 0)
  e0_lo <- modify_life_table(L_romania, D_romania, cod_change = -50)
  e0_hi <- modify_life_table(L_romania, D_romania, cod_change = 50)

  # zero change reproduces the original life table
  expect_equal(e0_0$ex[1], L_romania$ex[1], tolerance = 1e-2)

  # reducing mortality raises LE; increasing it lowers LE
  expect_gt(e0_lo$ex[1], e0_0$ex[1])
  expect_lt(e0_hi$ex[1], e0_0$ex[1])

  # output has the same structure as the input life table
  expect_equal(names(e0_lo), names(L_romania))
  expect_equal(nrow(e0_lo), nrow(L_romania))
})

test_that("exFUN returns the life expectancy from a cause-specific mortality matrix", {
  # a proper mortality matrix is the cause shares scaled by the all-cause mx
  rates <- build_cod_matrix(D_romania) * L_romania$mx
  ex <- exFUN(L_romania$x, rates)

  expect_true(is.finite(ex))
  expect_gt(ex, 0)
  expect_equal(ex, L_romania$ex[1], tolerance = 1e-2)

  # lower mortality -> higher life expectancy (zero matrix gives Inf)
  ex_zero <- exFUN(L_romania$x, matrix(0, nrow = 25, ncol = 18))
  expect_true(is.infinite(ex_zero))
})

test_that("decompose_by_age reproduces the life expectancy gap", {
  dec <- decompose_by_age(L_romania, L_mexico)

  expect_s3_class(dec, "decompose")
  expect_true(all(c("region", "period", "sex", "x.int", "x", "decomposition") %in% names(dec)))
  expect_equal(nrow(dec), 25)
  # identification columns carry labels, not factor integer codes
  expect_equal(unique(dec$sex), "both")
  expect_equal(unique(dec$region), "Romania - Mexico")

  gap <- L_mexico$ex[1] - L_romania$ex[1]
  expect_equal(sum(dec$decomposition), gap, tolerance = 1e-3)
})

test_that("decompose_by_cod reproduces the gap and is symmetric", {
  dec  <- decompose_by_cod(L_romania, L_mexico, D_romania, D_mexico)
  dec2 <- decompose_by_cod(L_mexico, L_romania, D_mexico, D_romania)

  expect_s3_class(dec, "decompose")
  expect_true("cause_name" %in% names(dec))
  expect_equal(nrow(dec), 25 * length(unique(D_romania$cause_name)))
  # identification columns carry labels, not factor integer codes
  expect_equal(unique(dec$sex), "both")
  expect_equal(unique(dec$region), "Romania - Mexico")

  gap <- L_mexico$ex[1] - L_romania$ex[1]
  expect_equal(sum(dec$decomposition), gap, tolerance = 1e-3)

  # swapping the two populations negates the total contribution
  expect_equal(sum(dec$decomposition) + sum(dec2$decomposition), 0, tolerance = 1e-3)
})

test_that("matrix_to_long_table converts a matrix to a long table", {
  X <- matrix(c(0.1, 0.2, 0.3, 0.4),
              nrow = 2,
              dimnames = list(c("0", "10"), c("Stroke", "COVID-19")))
  out <- matrix_to_long_table(X, D_romania, D_mexico)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 4)
  expect_true(all(c("region", "period", "sex", "x.int", "x", "cause_name", "decomposition") %in% names(out)))
  expect_equal(sort(out$decomposition), c(0.1, 0.2, 0.3, 0.4))
  # identification columns come from C1/C2; differing regions are merged
  expect_equal(unique(out$region), "Romania - Mexico")
  expect_equal(unique(out$sex), "both")
})

test_that("prepare_data handles all three comparison modes", {
  # small inputs: Romania + Mexico, both sexes, 2021
  D_cmp <- D_full[D_full$region %in% c("Romania", "Mexico") &
                    D_full$sex == "both" & D_full$period == 2021, ]
  L_cmp <- L_full[L_full$region %in% c("Romania", "Mexico") &
                    L_full$sex == "both" & L_full$period == 2021, ]
  # male + female Romania only, for the sex mode
  D_sex <- D_full[D_full$region == "Romania" & D_full$period == 2021, ]
  L_sex <- L_full[L_full$region == "Romania" & L_full$period == 2021, ]

  # -- single region, no change
  out0 <- prepare_data(D_cmp, L_cmp, "Romania", "Romania", "both", 0, mode = "cod")
  expect_equal(names(out0), c("cod_initial", "cod_final", "lt_initial", "lt_final"))
  expect_identical(out0$cod_initial, out0$cod_final)
  expect_identical(out0$lt_initial, out0$lt_final)

  # -- single region, -50% change: COD deaths halve and LE rises
  out50 <- prepare_data(D_cmp, L_cmp, "Romania", "Romania", "both", -50, mode = "cod")
  expect_false(isTRUE(all.equal(out50$cod_initial$deaths, out50$cod_final$deaths)))
  expect_gt(out50$lt_final$ex[1], out50$lt_initial$ex[1])

  # -- country comparison: two regions, factor levels set
  outc <- prepare_data(D_cmp, L_cmp, "Romania", "Mexico", "both", 0, mode = "cntr")
  expect_equal(as.character(unique(outc$cod_initial$region)), "Romania")
  expect_equal(as.character(unique(outc$cod_final$region)), "Mexico")
  expect_equal(levels(outc$cod_initial$region), c("Romania", "Mexico"))

  # -- sex comparison: male vs female within one region
  outs <- prepare_data(D_sex, L_sex, "Romania", "Romania", "both", 0, mode = "sex")
  expect_equal(as.character(unique(outs$cod_initial$sex)), "male")
  expect_equal(as.character(unique(outs$cod_final$sex)), "female")
})

test_that("dt_filter_local filters by region, sex and year", {
  # single region
  r <- dt_filter_local(D_full, "mode_cod", "Romania", "Romania", "both", 2021, NULL)
  expect_equal(nrow(r), 450)
  expect_s3_class(r, "tbl_df")

  # two regions
  r2 <- dt_filter_local(D_full, "mode_cntr", "Romania", "Mexico", "both", 2021, NULL)
  expect_equal(nrow(r2), 900)

  # sex mode bypasses the gender filter (returns all sexes)
  r3 <- dt_filter_local(D_full, "mode_sex", "Romania", "Romania", "female", 2021, NULL)
  expect_equal(nrow(r3), 1350)  # 3 sexes x 450
  expect_setequal(unique(r3$sex), c("both", "female", "male"))

  # a plain data.frame input is converted internally
  r4 <- dt_filter_local(as.data.frame(D_full), "mode_cod", "Romania", "Romania", "both", 2021, NULL)
  expect_equal(nrow(r4), 450)
  expect_s3_class(r4, "tbl_df")
})
