# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Thu Sep 09 22:35:11 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)
library(rnaturalearth)
library(countrycode)
library(sf)
library(wpp2019)


data(pop)
data(e0F)
data(e0M)
data(tfr)
data(sexRatio)


data_wpp <- select(pop, name) %>% 
  mutate(
    pop = pop$`2020` * 1000,
    e0F = e0F$`2015-2020`,
    e0M = e0M$`2015-2020`,
    sexRatio = sexRatio$`2015-2020`,
    tfr = tfr$`2015-2020`,
  )


data_sf <- rnaturalearth::ne_countries(
  scale = "small", 
  type = "countries", 
  returnclass = "sf") %>% 
  select(name, iso_a3, iso_a2) %>% 
  mutate(continent = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent"),
         region = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region")) %>% 
  filter(str_detect(name, "Antarctic", negate = TRUE)) %>%
  cbind(st_coordinates(st_centroid(., of_largest_polygon = TRUE))) %>% 
  rename(lon = X, lat = Y) %>% 
  left_join(., data_wpp, by = "name")


usethis::use_data(data_sf, overwrite = TRUE)










