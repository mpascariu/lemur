# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Thu Mar 17 17:33:46 2022
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)
library(readxl)

load("data-raw/data_gbd2019_cod_20220317.Rdata")
load("data-raw/data_gbd2019_sdg_20220317.Rdata")

path_map   <- paste0(getwd(),"/data-raw/GBD_2019_Data_Tools_Guide/")
file_map   <- "IHME_GBD_2019_A1_HIERARCHIES_Y2020M10D15.XLSX"
super_regions <- read_excel(
  path = paste0(path_map, file_map),
  sheet = "All Locations Hierarchies") %>% 
  filter(
    Download == "yes",
    Level <= 1
    ) %>% 
  select(`Location Name`) %>% 
  unlist() %>% 
  as.character() %>% 
  toupper()

D <- data_gbd2019_cod

all_regions <- levels(D$region)
countries <- all_regions[!(all_regions %in% super_regions)]



data_app_input <- list(
  regions    = super_regions,
  countries  = countries,
  cause_name = levels(D$cause_name),
  period = unique(D$period),
  sex    = unique(D$sex),
  x      = unique(D$x)
)


data_app_input$cause_name_sdg <- levels(data_gbd2019_sdg$cause_name)


usethis::use_data(data_app_input, overwrite = TRUE)
