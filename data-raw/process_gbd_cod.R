# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: 2026-08-31
# ------------------------------------------------- #
#
# Cause-of-death data, GBD 2021 (1990-2021) + GBD 2023 (2023) combined.
#
# Same approach as the former process_gbd2021_cod.R, except:
#   * no pclm ungroup of the 95+ interval -- 95+ stays an open group (x = 95);
#   * macro regions (Africa/Americas/Asia/Europe) and the Korea relabel are
#     applied by the shared gbd_utils.R, and 2023 rows are appended;
#   * 2021-round deaths cover 1990-2021, 2023-round deaths cover 2023 only.
#
# SELECTION (cause groups) -- see IHME_GBD_2021_A1_HIERARCHIES_Y2024M05D15.XLSX,
# sheet "Cause Hierarchy", column `cod_selection`.

source("data-raw/gbd_utils.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(readxl)
  library(janitor)
})

# ---- mapping tables -------------------------------------------------------

hier <- read_hierarchy()
loc_map    <- build_location_map(hier$loc)
cod_map    <- hier$cod %>% filter(cod_selection != "no") %>% select(cause_id, cod_selection)
cod_rank   <- hier$cod %>%
  filter(cod_selection != "no", cod_selection != "COVID-19 (2)") %>%
  arrange(cod_order) %>% pull(cod_selection) %>% unique()

# Level_3 subgroups to report alongside their Level_2 parents.
lev3_cod <- c("COVID-19",
              "Colon and rectum cancer",
              "Tracheal, bronchus, and lung cancer",
              "Ischemic heart disease",
              "Stroke")

# ---- read + shape both rounds ----------------------------------------------

raw21 <- read_deaths_2021(lev3_cod)          # 2021-round deaths, 1990-2021
raw23 <- read_deaths_2023()                  # 2023-round deaths, 2023
gbd21 <- shape_deaths(raw21, loc_map)
gbd23 <- shape_deaths(raw23, loc_map)

# reference totals (all-cause row, cause id 294) for the consistency check
ref21 <- allcause_reference(raw21, loc_map)
ref23 <- allcause_reference(raw23, loc_map)

# ---- map causes and aggregate -----------------------------------------------

prep <- function(d) {
  d %>%
    left_join(cod_map, by = "cause_id") %>%
    # keep every mapped cause (including "COVID-19 (2)", which the reduction
    # folds into "COVID-19" before the (2) row is dropped) -- filtering to
    # cod_rank here would discard those deaths early.
    filter(!is.na(cod_selection), cod_selection != "no") %>%
    group_by(region, sex, period, x, cause_name = cod_selection) %>%
    summarise(deaths = sum(deaths), .groups = "drop")
}

# The Level_3 subcategories live inside their Level_2 parents too, so subtract
# them back out to avoid double counting. sum() over an absent cause is 0, which
# keeps 2023 (no "COVID-19 (2)") well-behaved.
reduce_cod <- function(d) {
  d %>%
    group_by(region, sex, period, x) %>%
    mutate(
      deaths = ifelse(cause_name == "Other Cardiovascular",
                      deaths - sum(deaths[cause_name == "Ischemic Heart Disease"])
                              - sum(deaths[cause_name == "Stroke"]),
                      deaths),
      deaths = ifelse(cause_name == "Other Neoplasms",
                      deaths - sum(deaths[cause_name == "Colon and Rectum Cancer"])
                              - sum(deaths[cause_name == "Lung Cancer"]),
                      deaths),
      deaths = ifelse(cause_name == "Respiratory Infections (excl. COVID)",
                      deaths - sum(deaths[cause_name == "COVID-19"]),
                      deaths),
      deaths = ifelse(cause_name == "COVID-19",
                      deaths + sum(deaths[cause_name == "COVID-19 (2)"]),
                      deaths)
    ) %>%
    ungroup()
}

gbd <- bind_rows(
  prep(gbd21) %>% reduce_cod(),
  prep(gbd23) %>% reduce_cod()
) %>%
  filter(cause_name != "COVID-19 (2)")

# ---- sexes combined + complete the grid -------------------------------------

GBD <- gbd %>%
  add_both_sex() %>%
  finalise(cod_rank)

# ---- consistency checks -----------------------------------------------------

# 1. Every region x sex x period combination must be present for every x+cause;
#    finalise() enforces it, so NA is the only remaining worry.
stopifnot(sum(is.na(GBD$deaths)) == 0)

# 2. All-cause totals: sum of the cause-specific deaths must reproduce the
#    "All causes" row (cause id 294) for the same region/sex/period/age.
check_allcause <- function(d, ref, label) {
  tot <- d %>%
    filter(sex != "both") %>%
    group_by(region, sex, period, x) %>%
    summarise(deaths_sum = sum(deaths), .groups = "drop") %>%
    left_join(ref, by = c("region", "sex", "period", "x"))
  bad <- tot %>% mutate(delta = deaths_sum - deaths_ref) %>%
    filter(abs(delta) > 1)
  cat(sprintf("  [%s] all-cause consistency rows checked: %d, delta>1 rows: %d\n",
              label, nrow(tot), nrow(bad)))
  if (nrow(bad)) print(bad %>% slice_max(abs(delta), n = 5))
  invisible(bad)
}
check_allcause(GBD, bind_rows(ref21, ref23), "cod")

# 3. both == male + female at every (region, period, x, cause)
both_ok <- GBD %>%
  spread(sex, deaths) %>%
  filter(abs((male + female) - both) > 0.01)
cat(sprintf("  [cod] both==male+female rows failing: %d\n", nrow(both_ok)))

# ---- write -------------------------------------------------------------------

data_gbd_cod <- GBD %>% drop_na()
dt <- format(Sys.Date(), "%Y%m%d")
save(data_gbd_cod, file = file.path("data-raw", "IHME_GBD2021_Data",
                                    paste0("data_gbd_cod_", dt, ".Rdata")))

cat("\nCombined COD write OK:\n")
print(data_gbd_cod %>%
        group_by(period, sex) %>% summarise(regions = n_distinct(region)) %>%
        as.data.frame())
cat("n causes:", nlevels(data_gbd_cod$cause_name),
    "| age grid:", paste(sort(unique(data_gbd_cod$x)), collapse = ","), "\n")
