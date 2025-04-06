# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Sun Apr  6 20:20:48 2025
# ------------------------------------------------- #

remove(list = ls())
library(tidyverse)
library(rnaturalearth)
library(giscoR)
library(sf)


# READ countries mapping -----------------------------------------

path_map <- paste0(getwd(),"/data-raw/GBD_2021_Data_Tools_Guide/")
file_map <- "IHME_GBD_2021_A1_HIERARCHIES_Y2024M05D15.XLSX"
path_file_map <- paste0(path_map, file_map)

region_map <- read_excel(
  path = path_file_map,
  sheet = "GBD 2021 Locations Hierarchy") %>% 
  clean_names() %>% 
  select(iso3_code, location_name, location_id) %>% 
  rename(region = location_name)

# Read population data ------------------------------------------
path      <- paste0(getwd(),"/data-raw/IHME_GBD2021_Data")
files     <- list.files(path, full.names = TRUE, recursive = TRUE)
files_zip <- grep(".zip", files, value = TRUE)
files_pop <- grep("Population", files, value = TRUE)

gbd_pop <- files_pop %>%
  # read in all the files individually, using
  # the function read_csv() from the readr package
  map(read_csv) %>%
  # reduce with rbind into one dataframe
  reduce(rbind) %>% 
  # add the region names from region_map based on location ID
  select(location_id, val) %>% 
  mutate(val = round(val, 0)) %>% 
  left_join(region_map, by = "location_id") %>% 
  select(region, val) %>% 
  rename(population_2021 = val)


# Read Total fertility rate data --------------------------------
files_fertility <- grep("Fertility", files, value = TRUE)

gbd_fertility <- files_fertility %>%
  map(read_csv) %>%
  reduce(rbind) %>% 
  select(location_id, val) %>% 
  mutate(val = round(val, 2)) %>% 
  left_join(region_map, by = "location_id") %>% 
  select(region, val) %>% 
  rename(tfr_2021 = val)

# Read Life Expectancy data --------------------------------------
# We are taking this from the values calculated with <process_gbd2021_lt.R>

files_zip <- grep(".Rdata", files, value = TRUE)
files_lt <- grep("_lt_", files, value = TRUE)

load(files_lt)

gbd_lt <- data_gbd2021_lt %>% 
  filter(
    x == 0,
    period == 2021,
    ) %>% 
  select(region, sex, ex) %>% 
  mutate(ex = round(ex, 2)) %>% 
  pivot_wider(names_from = sex, values_from = ex) %>% 
  rename(
    e0m_2021 = male,
    e0f_2021 = female,
    e0b_2021 = both
    )

# POLYGONS -----------------------------------------------------------
data("gisco_countries")

world_polygons <- gisco_countries %>% 
  select(ISO3_CODE) %>% 
  clean_names() 

taiwan_polygons <- ne_countries(scale = "small", returnclass = "sf") %>% 
  select(iso_a3) %>%
  rename(iso3_code = iso_a3) %>% 
  filter(iso3_code == "TWN")

data_sf <- bind_rows(world_polygons, taiwan_polygons) %>% 
  # Append additional information: population size, fertility, and life expectancy
  left_join(., region_map, by = "iso3_code") %>% 
  left_join(., gbd_pop, by = c("region")) %>% 
  left_join(., gbd_fertility, by = c("region")) %>% 
  left_join(., gbd_lt, by = c("region")) %>% 
  cbind(st_coordinates(st_centroid(., of_largest_polygon = TRUE))) %>% 
  rename(name = region, lon = X, lat = Y) %>% 
  filter(!is.na(name)) 


dt <- format(Sys.Date(), '%Y%m%d')
save(data_sf, file = paste0("data-raw/IHME_GBD2021_Data/data_sf_", dt,".Rdata"))
usethis::use_data(data_sf, overwrite = TRUE)

# ----------------------------------------------------------------------------




