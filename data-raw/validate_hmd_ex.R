# ------------------------------------------------- #
# Validate lemur life tables against the Human Mortality Database
#
# Compares life expectancy at birth (e0) and at age 65 (e65) from the
# package's GBD-based life tables (inst/extdata/lt_dt.rds) against HMD
# summary indicators (hmd_summary_ex_0_65_80.xlsx, last modified 27 Aug 2026),
# for all HMD countries with a GBD counterpart, both sexes, 2021 and 2023.
#
# Output: console report + docs/figures/hmd_validation.png
# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: 2026-09-02
# ------------------------------------------------- #

suppressPackageStartupMessages({
  library(readxl)
  library(data.table)
})

HMD_XLSX <- "data-raw/IHME_GBD2023_Data/hmd_summary_ex_0_65_80.xlsx"
YEARS    <- c(2021, 2023)
AGES     <- c(0, 65)

# ---- 1. HMD summary workbook -> long format ---------------------------------

sheets <- excel_sheets(HMD_XLSX)
sheets <- sheets[sheets != "Introduction"]

hmd <- rbindlist(lapply(sheets, function(s) {
  d <- read_excel(HMD_XLSX, sheet = s, col_types = "text")
  countries <- as.character(unlist(d[1, -1]))
  d <- as.data.table(d[-1, ])
  setnames(d, 1, "year")   # col 1 holds the year; read_excel titles it after row 1
  m <- melt(d, measure.vars = paste0("...", seq_along(countries) + 1),
            variable.factor = FALSE, na.rm = TRUE)
  m[, `:=` (
    year     = as.integer(year),
    # col "...2" is the first data column, i.e. countries[1] — hence the -1
    country  = countries[as.integer(sub("...", "", variable, fixed = TRUE)) - 1],
    ex_hmd   = as.numeric(value)
  )]
  # sheet names look like "Female e65" / "Both-sex e0"
  parts <- strsplit(s, " ")[[1]]
  m[, sex := fifelse(tolower(parts[1]) == "both-sex", "both", tolower(parts[1]))]
  m[, age := as.integer(sub("e", "", parts[2], fixed = TRUE))]
  m[, .(year, country, sex, age, ex_hmd)]
}))

# ---- 2. Map HMD country names onto GBD region names --------------------------
# National-level HMD series only; sub-populations (Maori, East/West Germany,
# England & Wales, ...) and Hong Kong (absent from GBD) are dropped.

name_map <- c(
  "France: Total population"      = "France",
  "Germany: Total population"     = "Germany",
  "New Zealand: Total population" = "New Zealand",
  "Republic of Korea"             = "South Korea",
  "U.K.: United Kingdom Total Population" = "United Kingdom",
  "U.S.A."                        = "United States of America"
)

hmd[, region := fifelse(country %in% names(name_map),
                        unname(name_map[country]), country)]

lt <- readRDS("inst/extdata/lt_dt.rds")
gbd_regions <- unique(lt$region)
kept    <- sort(unique(hmd$region[hmd$region %in% gbd_regions]))
dropped <- sort(unique(hmd$country[!hmd$region %in% gbd_regions]))

cat(sprintf("HMD series kept: %d national populations | dropped: %s\n",
            length(kept), paste(dropped, collapse = ", ")))

hmd <- hmd[region %in% gbd_regions & year %in% YEARS & age %in% AGES]
cat(sprintf("HMD observations available: e0 = %d, e65 = %d (of %d possible each, %d regions x 3 sexes x 2 years)\n",
            sum(hmd$age == 0), sum(hmd$age == 65), length(kept) * 6, length(kept)))

# ---- 3. lemur life tables -> e0 / e65 ----------------------------------------

gbd <- lt[period %in% YEARS & x %in% AGES,
          .(region, period, sex, age = x, ex_gbd = ex)]

cmp <- merge(gbd, hmd, by.x = c("region", "sex", "age", "period"),
             by.y = c("region", "sex", "age", "year"))
cmp[, diff := ex_gbd - ex_hmd]   # GBD (lemur) minus HMD

# ---- 4. Report ---------------------------------------------------------------

fmt <- function(x) sprintf("%+.2f", x)

cat("\n=== Summary: GBD (lemur) minus HMD, years of life expectancy ===\n")
summ <- cmp[, .(
  n        = .N,
  mean     = mean(diff),
  mean_abs = mean(abs(diff)),
  min      = min(diff),
  max      = max(diff)
), by = .(age, sex, period)][order(age, sex, period)]
print(summ)

cat("\n=== Agreement bands (share of comparisons within +/- x years) ===\n")
bands <- cmp[, .(
  n        = .N,
  within_0.25 = mean(abs(diff) <= 0.25),
  within_0.5  = mean(abs(diff) <= 0.5),
  within_1.0  = mean(abs(diff) <= 1.0),
  within_2.0  = mean(abs(diff) <= 2.0)
), by = .(age)][order(age)]
print(bands)

cat("\n=== Largest deviations (top 12 by |diff|) ===\n")
worst <- cmp[order(-abs(diff))][1:12,
          .(region, period, sex, age,
            gbd = round(ex_gbd, 2), hmd = round(ex_hmd, 2),
            diff = round(diff, 2))]
print(worst)

# ---- 5. Figure: GBD vs HMD, per age, coloured by sex -------------------------

png("docs/figures/hmd_validation.png", width = 1000, height = 560, res = 110)
par(mfrow = c(1, 2), family = "",
    mar = c(4.5, 4.5, 3, 1), bg = "#fcfcfb")

sex_col <- c(female = "#eb6834", male = "#1baf7a")
sexes   <- c("female", "male")

for (a in AGES) {
  d <- cmp[age == a][sex %in% sexes]
  rng <- range(c(d$ex_gbd, d$ex_hmd))
  plot(NA, xlim = rng, ylim = rng, asp = 1,
       xlab = "HMD life expectancy (years)",
       ylab = "lemur / GBD life expectancy (years)",
       main = sprintf("e%d — %d population-sex-year points", a, nrow(d)),
       col.axis = "#52514e", col.lab = "#0b0b0b", col.main = "#0b0b0b",
       cex.main = 1.0, las = 1)
  abline(0, 1, col = "#c3c2b7", lwd = 2)          # identity line
  abline(h = par("usr")[3], v = par("usr")[1], col = "#c3c2b7", lwd = 0.5)
  grid(col = "#e8e7e4", lwd = 0.5)
  for (s in sexes) {
    dd <- d[sex == s]
    points(dd$ex_hmd, dd$ex_gbd, pch = 16, cex = 0.85, col = sex_col[s])
  }
  # label the largest deviations, one label per region; draw right of the point
  # only when the full text fits inside the panel, otherwise draw left of it
  w <- d[order(-abs(diff))][!duplicated(region)][1:3]
  fits <- w$ex_hmd + strwidth(w$region, cex = 0.7) * 1.2 < par("usr")[2]
  text(w$ex_hmd, w$ex_gbd, labels = w$region,
       pos = ifelse(fits, 4, 2), cex = 0.7, col = "#52514e")
  if (a == AGES[1]) {
    legend("topleft", legend = sexes, pch = 16, col = sex_col[sexes],
           bty = "n", title = NULL, cex = 0.85)
  }
}
dev.off()

cat("\nFigure written to docs/figures/hmd_validation.png\n")

# ---- 6. Per-country markdown tables (for the study document) -----------------
# Four tables: e0/e65 x 2021/2023. One row per country, sexes side by side
# (HMD, lemur, and the difference lemur - HMD for each). An em dash marks an
# HMD value absent from the workbook.

grid <- as.data.table(expand.grid(region = kept,
                                  sex = c("female", "male"),
                                  period = YEARS))
for (a in AGES) {
  # GBD values exist for every country-year; HMD may not — so take GBD from the
  # life tables directly and only the HMD side from the inner-joined comparison
  d <- merge(grid, gbd[age == a], all.x = TRUE, by = c("region", "sex", "period"))
  d <- merge(d, hmd[, .(region, sex, age, period = year, ex_hmd)],
             all.x = TRUE, by = c("region", "sex", "age", "period"))
  d[, diff := ex_gbd - ex_hmd]

  for (p in YEARS) {
    w <- dcast(d[period == p], region ~ sex,
               value.var = c("ex_gbd", "ex_hmd", "diff"), sep = "_")
    setorder(w, region)

    cat(sprintf("\n### e%d, %d\n\n", a, p))
    cat("| Country | HMD (F) | lemur (F) | Diff (F) | HMD (M) | lemur (M) | Diff (M) |\n")
    cat("|---|---|---|---|---|---|---|\n")
    val <- function(x) if (is.na(x)) "—" else sprintf("%.2f", x)
    dfl <- function(x) if (is.na(x)) "—" else sprintf("%+.2f", x)
    for (i in seq_len(nrow(w))) {
      r <- w[i]
      cat(sprintf("| %s | %s | %s | %s | %s | %s | %s |\n",
                  r$region,
                  val(r$ex_hmd_female), val(r$ex_gbd_female), dfl(r$diff_female),
                  val(r$ex_hmd_male),   val(r$ex_gbd_male),   dfl(r$diff_male)))
    }
  }
}