# ------------------------------------------------- #
# Author: Marius D. Pascariu
# ------------------------------------------------- #
#
# Round-consistency study: GBD 2021 (downloaded 2025) vs GBD 2023 (2026).
#
# The bundled COD/SDG tables combine the 2021-round deaths (1990-2021) with the
# 2023-round deaths (2023); the life tables are 2023-round throughout. Before
# trusting that mix, we check two things:
#
#   A) ROUND REVISION (clean, same-year): the 2021-round life tables (backed up
#      in data-raw/_archive/) vs the 2023-round life tables for the SAME years
#      1990-2021. Any difference is pure GBD round revision of the method --
#      large shifts would signal the method/collection changed.
#
#   B) SERIES CONTINUITY at the 2021 -> 2023 seam (deaths). The combined series
#      jumps from the 2021-round year-2021 point to the 2023-round year-2023
#      point (no 2022). The 2021->2023 change should look like an ordinary
#      continuation of the 2019->2021 changes; an out-of-family jump suggests a
#      break. NOTE: this mixes round change with two real COVID-recovery years,
#      so it is suggestive, not a clean test -- A is the clean one.
#
# The report lives at docs/study_gbd_round_consistency.md.
#
# Usage: Rscript data-raw/study_round_consistency.R
# (Run from the package root; reads inst/extdata/*.rds and the gitignored
#  backup data-raw/_archive/gbd2021_2026-08-31/.)

suppressPackageStartupMessages(library(data.table))

COD <- readRDS("inst/extdata/cod_dt.rds")                       # combined deaths
LT_NEW <- readRDS("inst/extdata/lt_dt.rds")                     # 2023 round
LT_OLD <- readRDS("data-raw/_archive/gbd2021_2026-08-31/lt_dt.rds")  # 2021 round

setDT(COD); setDT(LT_NEW); setDT(LT_OLD)

# Case-study regions: high / middle / low income, aging & young populations,
# reported-name-stable names (no location relabel between rounds).
REGIONS <- c("Romania", "Mexico", "Japan", "United States of America",
             "Nigeria", "India", "Chile", "Sweden")
SEXES   <- c("both", "male", "female")
YEARS   <- sort(unique(LT_OLD$period))   # period years present in BOTH rounds (9)

# ---------------- Part A: round revision (life tables) ----------------
cat("\n================ PART A -- GBD round revision (same years, both rounds) ================\n")
cat("Delta = e2023-round - e2021-round, over years",
    paste(sort(unique(LT_OLD$period)), collapse = ","), "\n")

e0 <- function(lt, reg, sx, yr) lt[region == reg & sex == sx & period == yr & x == 0, ex]
e65<- function(lt, reg, sx, yr) lt[region == reg & sex == sx & period == yr & x == 65, ex]

partA <- lapply(REGIONS, function(r) {
  lapply(SEXES, function(s) {
    d0  <- vapply(YEARS, function(y) e0(LT_NEW, r, s, y)  - e0(LT_OLD, r, s, y), numeric(1))
    d65 <- vapply(YEARS, function(y) e65(LT_NEW, r, s, y) - e65(LT_OLD, r, s, y), numeric(1))
    data.frame(region = r, sex = s,
               mean_abs_d_e0 = round(mean(abs(d0)), 3),
               max_abs_d_e0 = round(max(abs(d0)), 3),
               d_e0_2015  = round(e0(LT_NEW, r, s, 2015)  - e0(LT_OLD, r, s, 2015), 3),
               d_e0_2021  = round(e0(LT_NEW, r, s, 2021)  - e0(LT_OLD, r, s, 2021), 3),
               mean_abs_d_e65 = round(mean(abs(d65)), 3),
               flag = if (max(abs(d0)) > 0.5) "LABGE" else "ok")
  })
})
print(do.call(rbind, unlist(partA, recursive = FALSE)), row.names = FALSE)

# ---------------- Part B: death-series continuity at the 2021->2023 seam ----------------
cat("\n================ PART B -- all-cause deaths, combined series (both sexes) ================\n")
allc <- COD[, .(deaths = sum(deaths)), by = .(region, sex, period)]
g <- function(dd, y1, y2) (dd(y2)/dd(y1))^(1/(y2 - y1)) - 1   # annualized growth

partB <- lapply(REGIONS, function(r) {
  d <- allc[region == r & sex == "both"]
  dd <- function(y) d[period == y, deaths]
  data.frame(region = r,
             D_2019 = round(dd(2019)), D_2020 = round(dd(2020)),
             D_2021 = round(dd(2021)), D_2023 = round(dd(2023)),
             g_1921 = round(100*g(dd, 2019, 2021), 1),   # %/yr 2019->2021
             g_2123 = round(100*g(dd, 2021, 2023), 1),   # %/yr 2021->2023
             ratio = round(dd(2023)/dd(2019), 3))
})
print(do.call(rbind, partB), row.names = FALSE)
cat("\n  g_* are annualised growth (%/yr). ratio = D2023/D2019.\n")
cat("  A 2021->2023 annualised change far outside the 2019->2021 range flags a suspicion.\n")

# ---------------- Part C: age structure at the seam ----------------
cat("\n================ PART C -- age-specific all-cause deaths (both sexes) ================\n")
AGES <- c(0, 30, 65, 90)   # <1, 30-34, 65-69, 90-94
ag <- COD[sex == "both", .(deaths = sum(deaths)), by = .(region, period, x)]
ag <- ag[x %in% AGES]
for (r in REGIONS) {
  d <- ag[region == r]
  cat("\n", r, " (death counts; ratio = 2023/2021 per age group)\n")
  wide <- dcast(d, x ~ period, value.var = "deaths")
  wide[, ratio := round(`2023`/`2021`, 2)]
  print(wide, row.names = FALSE)
}

cat("\nDone. See docs/study_gbd_round_consistency.md for the write-up.\n")
