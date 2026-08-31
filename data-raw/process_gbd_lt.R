# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: 2026-08-31
# ------------------------------------------------- #
#
# Abridged life tables, 1990-2023, built from the GBD 2023-round probability
# of death (life-table "qxn") download.
#
# Differences from the former process_gbd2021_lt.R:
#   * source is the 2023-round qxn (IHME-GBD_LT_*.zip), one consistent round
#     for the whole table, covering 1990-2023;
#   * no Kannisto extension to 110 -- the terminal 95+ interval is left open
#     using GBD's own value (q(95+) = 1) and the default open-interval ax;
#   * macro regions (Africa/Americas/Asia/Europe) and the Korea relabel are
#     applied by the shared gbd_utils.R.

source("data-raw/gbd_utils.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(MortalityLaws)
})

hier    <- read_hierarchy()
loc_map <- build_location_map(hier$loc)

# ---- read + shape the qxn data ---------------------------------------------

qxn <- read_lt_2023() %>%
  filter(measure_name == "Life table") %>%
  left_join(loc_map, by = "location_id") %>%
  filter(!is.na(region), !location_id %in% MACRO_DROP_IDS) %>%
  mutate(x = age_to_x(age_name), sex = tolower(sex_name), period = year) %>%
  filter(!is.na(x)) %>%
  select(region, sex, period, x, qxn = val)

x_full <- c(0, 1, 2, seq(5, 95, 5))   # terminal 95+ is open

cases <- qxn %>% select(region, sex, period) %>% distinct()

# Every region x sex x period must supply the full 22-age qx vector; report any
# incomplete ones before building.
qxn_wide <- qxn %>% spread(x, qxn)
incomplete <- qxn_wide %>%
  rowwise() %>%
  mutate(n_ages = sum(!is.na(c_across(all_of(as.character(x_full)))))) %>%
  filter(n_ages < length(x_full)) %>% ungroup()
if (nrow(incomplete)) {
  cat("Incomplete qxn schedules (skipped):\n"); print(incomplete)
}

LTS <- lapply(seq_len(nrow(cases)), function(i) {
  S <- cases[i, ]
  d <- qxn %>% filter(region == S$region, sex == S$sex, period == S$period) %>% arrange(x)
  qx <- d$qxn
  # The GBD 95+ value is already the terminal open-interval q (=1); force it to
  # be safe before constructing.
  qx[length(qx)] <- 1
  lt <- MortalityLaws::LifeTable(
    x   = x_full,
    qx  = qx,
    sex = ifelse(S$sex == "both", "total", S$sex)
  )$lt %>%
    mutate(region = S$region, period = S$period, sex = S$sex, .before = 1)
  lt
})

data_gbd_lt <- as_tibble(bind_rows(LTS)) %>%
  mutate(region = factor(region))

# CHECK: no NA life-table values
na_rows <- data_gbd_lt %>% filter(is.na(mx) | is.na(ex))
cat("LT rows with NA mx/ex:", nrow(na_rows), "\n")
stopifnot(nrow(na_rows) == 0)

# ---- write -------------------------------------------------------------------

dt <- format(Sys.Date(), "%Y%m%d")
save(data_gbd_lt, file = file.path("data-raw", "IHME_GBD2021_Data",
                                   paste0("data_gbd_lt_", dt, ".Rdata")))

cat("\nCombined LT write OK:\n")
print(data_gbd_lt %>%
        group_by(period, sex) %>% summarise(regions = n_distinct(region)) %>%
        as.data.frame())
cat("age grid:", paste(sort(unique(data_gbd_lt$x)), collapse = ","),
    "| cols:", paste(names(data_gbd_lt), collapse = ", "), "\n")
