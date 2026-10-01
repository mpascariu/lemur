# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: 2026-08-31
# ------------------------------------------------- #

# Builds the app's input metadata (data_app_input): the region / country
# dropdown lists, the cause levels for the COD and SDG grouping, the period
# axis and the age grid. Must stay in sync with process_gbd_*.R (same region
# mapping and cause ranks, age grid capped at 95+, periods through 2023).

source("data-raw/gbd_utils.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(readxl)
})

hier     <- read_hierarchy()
loc_map  <- build_location_map(hier$loc)
region_map <- hier$loc   # carries the `type` column

# Macro regions present in 2021 but not 2023 (Africa/Americas/Asia/Europe) are
# dropped to keep the two rounds consistent; Korea labels fixed symmetrically.
super_regions <- region_map %>%
  filter(type == "region") %>%
  left_join(loc_map, by = "location_id") %>%
  filter(!location_id %in% MACRO_DROP_IDS) %>%
  pull(region) %>%
  unique()

countries <- region_map %>%
  filter(type == "country") %>%
  left_join(loc_map, by = "location_id") %>%
  pull(region) %>%
  sort()

cod_selection <- hier$cod %>%
  filter(cod_selection != "no", cod_selection != "COVID-19 (2)") %>%
  arrange(cod_order) %>% pull(cod_selection) %>% unique()

sdg_selection <- hier$cod %>%
  filter(sdg_selection != "no") %>%
  arrange(sdg_order) %>% pull(sdg_selection) %>% unique()

data_app_input <- list(
  regions    = super_regions,
  countries  = countries,
  cause_name     = factor(cod_selection, levels = cod_selection),
  cause_name_sdg = factor(sdg_selection, levels = sdg_selection),
  period = c(seq(1990, 2015, 5), 2019, 2020, 2021, 2023),
  sex    = c("male", "female", "both"),
  x      = c(0, 1, 2, seq(5, 95, 5))
)

usethis::use_data(data_app_input, overwrite = TRUE)
