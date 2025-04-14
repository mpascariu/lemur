# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Mon Apr 14 22:33:07 2025
# ------------------------------------------------- #


remove(list = ls())
library(tidyverse)
library(janitor)
library(readxl)

path_map <- paste0(getwd(),"/data-raw/GBD_2021_Data_Tools_Guide/")
file_map <- "IHME_GBD_2021_A1_HIERARCHIES_Y2024M05D15.XLSX"
path_file_map <- paste0(path_map, file_map)

region_map <- read_excel(
  path = path_file_map,
  sheet = "GBD 2021 Locations Hierarchy") %>% 
  clean_names()

super_regions <- region_map %>% 
  filter(type == "region") %>% 
  select(location_name) %>% 
  unlist() %>% 
  toupper() %>% 
  unname() 

countries <- region_map %>% 
  filter(type == "country") %>% 
  select(location_name) %>% 
  unlist() %>% 
  unname() %>% 
  sort()

cod_map <- read_excel(
  path = path_file_map,
  sheet = "Cause Hierarchy") %>% 
  clean_names()

cause_name <- cod_map %>% 
  filter(
    cod_selection != "no",
    cod_selection != "COVID-19 (2)"
    ) %>%
  arrange(cod_order) %>% 
  select(cod_selection) %>% 
  unlist() %>% 
  unname() %>% 
  unique() 

cause_name_sdg <- cod_map %>% 
  filter(
    sdg_selection != "no",
    ) %>%
  arrange(sdg_order) %>% 
  select(sdg_selection) %>% 
  unlist() %>% 
  unname() %>% 
  unique() 


data_app_input <- list(
  regions    = super_regions,
  countries  = countries,
  cause_name = factor(cause_name, levels = cause_name),
  cause_name_sdg = factor(cause_name_sdg, levels = cause_name_sdg),
  period = c(seq(1990, 2015, 5), 2019, 2020, 2021),
  sex    = c("male", "female", "both"),
  x      = c(0, 1, 2, seq(5, 110, 5))
)



usethis::use_data(data_app_input, overwrite = TRUE)
