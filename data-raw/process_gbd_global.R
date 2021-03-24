# --------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Tue Mar 23 20:01:42 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)

# The data have been obtained form GBD tool:
# Global Burden of Disease Collaborative Network.
# Global Burden of Disease Study 2019 (GBD 2019) Results.
# Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020.
# Available from http://ghdx.healthdata.org/gbd-results-tool.

# SELECTION
# Base: Single
# Location: Global (1)
# Year: 2015; ...; 2019  (5)
# Context: Cause (1)
# Age: All Ages; <1; 1 to 4; 5 to 9; ...; 90 to 94; 95 plus (22)
# Metric: Number (1)
# Measure: Deaths (1)
# Sex: Male; Female (2)
# Cause: select only level 2 causes; Total all causes (23)
# 
# IHME data request - confirmation: 2021-03-23 20:05
# IHME data request - file(s) ready for download: 2021-03-23 20:05

gbd_global <- read_csv(file = "data-raw/GBD-COD-data/IHME-GBD_2019_DATA-Global.zip")

gbd_global$year %>% unique()

gbd <- gbd_global %>% 
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
    x = ifelse(age_name == "All Ages", 999, x),  # code 999 for 'All Ages'
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

gbd


# Compute figures for both sexes
gbd_both <- gbd %>% 
  # group-by everything except 'deaths' and 'sex' columns
  group_by_at(setdiff(names(.), c("deaths", "sex"))) %>% 
  # aggregate data across sexes
  summarise(deaths = sum(deaths)) %>%
  # add sex column in the 3rd positon 
  add_column(sex = "Both", .after = 2) %>% 
  # remove group_by to avoid future trouble
  ungroup()


# Create the big dataset with 3 sexes
GBD <- bind_rows(gbd, gbd_both) %>% 
  # compute percentages of each disease for given region-period-sex and across ages
  group_by(region, period, sex) %>% 
  mutate(perc = deaths / sum(deaths)) %>% 
  ungroup()


# ------------------------------------------
# CHECKS



# 1. Check that the values across ages sum up to the total
GBD %>% 
  filter(x == 999) %>% 
  select(deaths) %>% 
  sum()

GBD %>% 
  filter(x != 999) %>% 
  select(deaths) %>% 
  sum()

# 2. Check the same thing by sex and cod
GBD %>% 
  filter(x == 999) %>% 
  group_by(sex, cause_name) %>% 
  summarise(deaths = sum(deaths))

GBD %>% 
  filter(x != 999) %>% 
  group_by(sex, cause_name) %>% 
  summarise(deaths = sum(deaths))

































