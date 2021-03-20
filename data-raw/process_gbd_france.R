# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# License: GNU General Public License v3.0
# Last update: Sat Mar 20 11:31:37 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)
library(readr)


# The data have been obtained form GBD tool:
# Global Burden of Disease Collaborative Network.
# Global Burden of Disease Study 2019 (GBD 2019) Results.
# Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020.
# Available from http://ghdx.healthdata.org/gbd-results-tool.

# Selection
# Base: Single
# Location: France, Japan, Tanzania (1)
# Year: 2015; ...; 2019  (5)
# Context: Cause
# Age: All Ages; <1; 1 to 4; 5 to 9; ...; 90 to 94 (21)
# Metric: Number (1)
# Measure: Deaths (1)
# Sex: Male; Female (2)
# Cause: select only level 3 causes (169)
# 
# IHME data request - confirmation: Sat 2021-03-20 12:00
# IHME data request - file(s) ready for download: Sat 2021-03-20 12:37


# read csv file
gbd_fra <- read_csv(file = "data-raw/GBD2019/IHME-GBD_2019_DATA-3C5Y169.zip")


gbd_fra$year %>% unique()

gbd <- gbd_fra %>% 
  select(
    # drop few columns
    location_name,
    sex_name,
    age_name,
    cause_id,
    cause_name,
    year,
    val) %>% 
  mutate(
    # create a numerical age column
    x = str_extract(age_name, "^.{2}"),
    x = ifelse(age_name == "<1 year", 0, x),
    x = as.numeric(x)) %>% 
  select(
    # drop few more
    - age_name
  ) %>% 
  rename(
    # rename few columns to follow the life table data
    region = location_name,
    sex = sex_name
  ) %>% 
  # group-by everything except 'val' and 'year' columns
  # so that we can aggregate data across years
  group_by_at(setdiff(names(.), c("val", "year"))) %>% 
  summarise(deaths = sum(val)) %>%
  # add period column in the second positon 
  add_column(period = "2015-2019", .after = 1) %>% 
  # remove group_by to avoid future trouble
  ungroup()


