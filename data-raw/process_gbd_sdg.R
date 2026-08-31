# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: 2026-08-31
# ------------------------------------------------- #
#
# Cause-of-death data grouped for UN SDG tracking, GBD 2021 (1990-2021) +
# GBD 2023 (2023) combined.
#
# Same approach and SDG grouping as the former process_gbd2021_sdg.R, but:
#   * no pclm ungroup of the 95+ interval (95+ stays open, x = 95);
#   * macro regions / Korea relabel applied by gbd_utils.R; 2023 appended.
# The SDG grouping uses more Level_3 causes than the COD grouping, so here the
# Level_3 read keeps the full SDG subcategory set.

source("data-raw/gbd_utils.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(readxl)
  library(janitor)
})

# ---- mapping tables ---------------------------------------------------------

hier <- read_hierarchy()
loc_map    <- build_location_map(hier$loc)
sdg_map    <- hier$cod %>% filter(sdg_selection != "no") %>% select(cause_id, sdg_selection)
sdg_rank   <- hier$cod %>%
  filter(sdg_selection != "no") %>%
  arrange(sdg_order) %>% pull(sdg_selection) %>% unique()

# Level_3 causes that the SDG grouping reports on their own (they sit inside
# Level_2 parents that the reductions subtract them back out of).
lev3_sdg <- c("Tuberculosis",
              "Malaria",
              "Diabetes mellitus",
              "Poisonings",
              "Exposure to forces of nature",
              "Self-harm",
              "Maternal disorders",
              "Neonatal disorders")

# ---- read + shape both rounds ----------------------------------------------

raw21 <- read_deaths_2021(lev3_sdg)
raw23 <- read_deaths_2023()
gbd21 <- shape_deaths(raw21, loc_map)
gbd23 <- shape_deaths(raw23, loc_map)

ref21 <- allcause_reference(raw21, loc_map)
ref23 <- allcause_reference(raw23, loc_map)

# ---- map causes and aggregate -----------------------------------------------

prep <- function(d) {
  d %>%
    left_join(sdg_map, by = "cause_id") %>%
    filter(sdg_selection %in% sdg_rank) %>%
    group_by(region, sex, period, x, cause_name = sdg_selection) %>%
    summarise(deaths = sum(deaths), .groups = "drop")
}

# The Level_3 subcategories live inside their Level_2 parents too; subtract the
# children back out of the parent total (sum() over an absent cause is 0).
reduce_sdg <- function(d) {
  d %>%
    group_by(region, sex, period, x) %>%
    mutate(
      deaths = ifelse(cause_name == "Respiratory Infections (excl. Tuberculosis)",
                      deaths - sum(deaths[cause_name == "Tuberculosis"]),
                      deaths),
      deaths = ifelse(cause_name == "Neglected Tropical Diseases (excl. Malaria)",
                      deaths - sum(deaths[cause_name == "Malaria"]),
                      deaths),
      deaths = ifelse(cause_name == "Kidney disease (excl. Diabetes)",
                      deaths - sum(deaths[cause_name == "Diabetes mellitus"]),
                      deaths),
      deaths = ifelse(cause_name == "Injuries (excl. Poisonings)",
                      deaths - sum(deaths[cause_name == "Poisonings"])
                              - sum(deaths[cause_name == "Exposure to forces of nature"]),
                      deaths),
      deaths = ifelse(cause_name == "Interpersonal Violence",
                      deaths - sum(deaths[cause_name == "Self-harm"]),
                      deaths)
    ) %>%
    ungroup()
}

gbd <- bind_rows(
  prep(gbd21) %>% reduce_sdg(),
  prep(gbd23) %>% reduce_sdg()
)

# ---- sexes combined + complete the grid -------------------------------------

GBD <- gbd %>%
  add_both_sex() %>%
  finalise(sdg_rank)

# ---- consistency checks -----------------------------------------------------

stopifnot(sum(is.na(GBD$deaths)) == 0)

check_allcause <- function(d, ref, label) {
  tot <- d %>%
    filter(sex != "both") %>%
    group_by(region, sex, period, x) %>%
    summarise(deaths_sum = sum(deaths), .groups = "drop") %>%
    left_join(ref, by = c("region", "sex", "period", "x"))
  bad <- tot %>% mutate(delta = deaths_sum - deaths_ref) %>% filter(abs(delta) > 1)
  cat(sprintf("  [%s] all-cause consistency rows checked: %d, delta>1 rows: %d\n",
              label, nrow(tot), nrow(bad)))
  if (nrow(bad)) print(bad %>% slice_max(abs(delta), n = 5))
  invisible(bad)
}
check_allcause(GBD, bind_rows(ref21, ref23), "sdg")

both_ok <- GBD %>%
  spread(sex, deaths) %>%
  filter(abs((male + female) - both) > 0.01)
cat(sprintf("  [sdg] both==male+female rows failing: %d\n", nrow(both_ok)))

# ---- write -------------------------------------------------------------------

data_gbd_sdg <- GBD %>% drop_na()
dt <- format(Sys.Date(), "%Y%m%d")
save(data_gbd_sdg, file = file.path("data-raw", "IHME_GBD2021_Data",
                                    paste0("data_gbd_sdg_", dt, ".Rdata")))

cat("\nCombined SDG write OK:\n")
print(data_gbd_sdg %>%
        group_by(period, sex) %>% summarise(regions = n_distinct(region)) %>%
        as.data.frame())
cat("n causes:", nlevels(data_gbd_sdg$cause_name),
    "| age grid:", paste(sort(unique(data_gbd_sdg$x)), collapse = ","), "\n")
