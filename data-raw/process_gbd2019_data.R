# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# License: GNU General Public License v3.0
# Last update: Tue Mar 16 22:42:43 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)
library(stringr)
library(readr)

# The data have been obtained form GBD tool:
# Global Burden of Disease Collaborative Network.
# Global Burden of Disease Study 2019 (GBD 2019) Results.
# Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020.
# Available from http://ghdx.healthdata.org/gbd-results-tool.

# read csv files
gbd_2015 <- read_csv(file = "data-raw/GBD2019/IHME-GBD_2019_DATA-50cef937-1.zip")
gbd_2016 <- read_csv(file = "data-raw/GBD2019/IHME-GBD_2019_DATA-a3938721-1.zip")
gbd_2017 <- read_csv(file = "data-raw/GBD2019/IHME-GBD_2019_DATA-3aed18e9-1.zip")
gbd_2018 <- read_csv(file = "data-raw/GBD2019/IHME-GBD_2019_DATA-3df82620-1.zip")
gbd_2019 <- read_csv(file = "data-raw/GBD2019/IHME-GBD_2019_DATA-5d0ba446-1.zip")


gbd <- bind_rows(
  # bind the 5 datasets
  gbd_2015,
  gbd_2016,
  gbd_2017,
  gbd_2018,
  gbd_2019) %>% 
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

# CHECK: Pick a country and see what we did
GBD %>% 
  filter(region == "Romania") %>% 
  select(perc) %>% 
  sum()  # this should be equal to 3. 100% for each sex. 




# ------------------------------------------
# Standardize country names
standard_country_names <- tibble(region = unique(GBD$region)) %>% 
  mutate(
    region_name = rangeBuilder::standardizeCountry(unique(region), fuzzyDist = 1, nthreads = 1),
    region_name = ifelse(region == "Bolivia (Plurinational State of)", "BOLIVIA", region_name),
    region_name = ifelse(region == "Micronesia (Federated States of)", "MICRONESIA", region_name),
    region_name = ifelse(region == "Taiwan (Province of China)", "TAIWAN", region_name),
    region_name = ifelse(region %in% c("Cabo Verde", 
                                       "Czechia", 
                                       "Eswatini", 
                                       "North Macedonia"),
                         toupper(region), region_name)
    )

GBD <- left_join(GBD, standard_country_names, by = "region") %>% 
  select(-region) %>% 
  rename(region = region_name) %>% 
  select(region, everything())

# check that we did not miss any country  
GBD %>% filter(region == "") 


# write.csv(unique(GBD$region), file = "data-raw/GBD2019/GBD_regions.csv")


# ------------------------------------------
meta = tibble(`Column Name` = colnames(GBD),
              Description = c("Region, subregion, country or area",
                              "Period of time",
                              "Sex",
                              "Cause ID code",
                              "Cause of death name",
                              "Age (x)",
                              "Death counts",
                              "Percentage"))

data_gbd2019 <- list(data = GBD,
                     meta = meta)

# include data in the package
usethis::use_data(data_gbd2019, overwrite = TRUE)

