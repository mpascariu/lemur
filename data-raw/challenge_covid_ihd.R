# ------------------------------------------------- #
# Author: Marius D. Pascariu
# ------------------------------------------------- #
#
# Challenge the 2021-round / 2023-round mix on COVID-19 and ischaemic heart
# disease, judged against the FULL 1990-2023 series -- not the 2015-23 window
# used initially.
#
# Two ideas drive it:
#   1. The pandemic shifted how deaths were recorded/attributed (excess
#      mortality, CVD<->COVID transfer), so 2020-21 is an anomalous anchor and
#      no seam conclusion should rest on it alone. Looking at the whole 1990-23
#      trend puts the 2021->23 step back in context.
#   2. For data-poor countries, cause counts jump around a lot WITHIN a single
#      round. So the right test of "is the seam special" is to compare the
#      seam change to the region's OWN within-round variability, not to an
#      idealised smooth trend.
#
# Usage: Rscript data-raw/challenge_covid_ihd.R   (package root)

suppressPackageStartupMessages(library(data.table))
C <- readRDS("inst/extdata/cod_dt.rds"); setDT(C)
dir.create("docs/figures", showWarnings = FALSE)

REGIONS <- c("Romania","Mexico","Japan","United States of America",
             "Nigeria","India","Chile","Sweden")
ihd <- C[cause_name == "Ischemic Heart Disease" & sex == "both",
         .(deaths = sum(deaths)), by = .(region, period)]
allc <- C[sex == "both", .(all = sum(deaths)), by = .(region, period)]
covid <- C[cause_name == "COVID-19" & sex == "both",
           .(deaths = sum(deaths)), by = .(region, period)]

# Highlighted locations: Japan and Romania are the data-rich outliers that the
# analysis singles out, so they get distinctive colours in every chart.
HILIT <- c(Japan = "steelblue", Romania = "firebrick")
linecol <- function(r) if (r %in% names(HILIT)) unname(HILIT[r]) else "grey50"
lw <- function(r) if (r %in% names(HILIT)) 3 else 1.5

# ---- Chart 1: IHD over the FULL period, 2019 = 100 ----
base <- ihd[period == 2019, .(region, b = deaths)]
rel <- merge(ihd, base, by = "region")[, rel := 100 * deaths / b]
png("docs/figures/ihd_fullperiod.png", width = 900, height = 560)
plot(NA, xlim = c(1990, 2023), ylim = c(40, 160),
     xlab = "Year", ylab = "IHD deaths (2019 = 100)",
     main = "Ischaemic heart disease deaths, full series, 2019 = 100\n(Japan, Romania highlighted)")
abline(v = 2022, lty = 3, col = "grey50")
for (r in REGIONS) {
  d <- rel[region == r][order(period)]
  lines(d$period, d$rel, col = linecol(r), lwd = lw(r))
}
legend("topleft", legend = c(names(HILIT), "other case regions"),
       col = c(unname(HILIT), "grey50"), lwd = c(3, 3, 1.5),
       cex = 0.8, bty = "n")
dev.off()

# ---- Chart 2: IHD % of all-cause by age, full period, per region ----
ihd_a <- C[cause_name == "Ischemic Heart Disease" & sex == "both",
           .(deaths = sum(deaths)), by = .(region, x, period)]
allc_a <- C[sex == "both", .(all = sum(deaths)), by = .(region, x, period)]
seg <- merge(allc_a, ihd_a, by = c("region","x","period"))
seg[, share := deaths / all * 100]
png("docs/figures/ihd_share_fullperiod.png", width = 1100, height = 700)
op <- par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
subreg <- c("Romania","Japan","Nigeria","United States of America","Mexico","India")
cols <- setNames(c("#E69F00", "#56B4E9", "#009E73"), c("50", "65", "80"))
for (r in subreg) {
  plot(NA, xlim = c(1990, 2023), ylim = c(0, 45),
       xlab = "Year", ylab = "IHD % of all-cause", main = r)
  for (a in c(50, 65, 80)) {
    dd <- seg[region == r & x == a][order(period)]
    lines(dd$period, dd$share, col = cols[as.character(a)], lwd = 2)
    px <- seg[region == r & x == a & period == 2021, share]
    if (length(px)) points(2021, px, col = cols[as.character(a)], pch = 16)
  }
  if (r == "Romania") legend("topleft", legend = c("age 50","age 65","age 80"),
                             col = cols, lwd = 2, cex = 0.7, bty = "n")
  if (r %in% c("Romania","Japan")) box(col = linecol(r), lwd = 3)   # key-finding panel
}
par(op); dev.off()

# ---- Diagnostic: is the seam change outside the region's OWN within-round band? ----
# within-round adjacent log-% changes over 2005..2021 (2021 round only) vs the
# 2021(2021 round)->2023(2023 round) seam.
inround <- c(2005, 2010, 2015, 2019, 2020, 2021)
both_round <- c(2019, 2021)
res <- lapply(unique(ihd$region), function(r) {
  v <- function(y) ihd[region == r & period == y, deaths]
  wc <- diff(log(sapply(inround, v))) * 100          # within round (%)
  pre <- diff(log(sapply(both_round, v))) * 100      # 2019->21 pre-seam
  seam <- 100 * (v(2023) / v(2021) - 1)
  data.frame(region = r,
             wr_min = min(wc), wr_max = max(wc),
             seam = seam,
             outside = abs(seam) > max(abs(wc), 0.5))
})
res <- do.call(rbind, res)
cat("\n===== IHD seam vs the region's OWN within-round (2005-21) band =====\n")
print(res[res$region %in% REGIONS, ])
cat(sprintf("\nAll 216 regions: IHD seam OUTSIDE own within-round band in %d/%d = %.0f%%\n",
            sum(res$outside), nrow(res), 100 * mean(res$outside)))

# for the write-up, list the data-rich outliers and confirm the poor-data story
# ---- Chart 3: COVID across the pandemic window only ----
# COVID-19 is identically zero before 2020, so a 1990-onwards axis is blank;
# restrict the chart to the years in which COVID-19 is non-zero (2019-2023).
png("docs/figures/covid_pandemic.png", width = 900, height = 560)
plot(NA, xlim = c(2019, 2023), ylim = c(0, max(covid$deaths)),
     xaxt = "n", xlab = "Year", ylab = "COVID-19 deaths (both sexes)",
     main = "COVID-19 deaths by region, pandemic window")
axis(1, at = c(2019, 2020, 2021, 2023))
for (r in REGIONS) {
  d <- covid[region == r][order(period)]
  d <- d[period >= 2019]
  lines(d$period, d$deaths, col = linecol(r), lwd = lw(r))
}
legend("topright", legend = c(names(HILIT), "other case regions"),
       col = c(unname(HILIT), "grey50"), lwd = c(3, 3, 1.5), cex = 0.8, bty = "n")
dev.off()

cat("\nFigures written to docs/figures/ (IHD series 1990-2023; COVID pandemic window 2019-2023).\n")
