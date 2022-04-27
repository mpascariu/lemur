# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed Nov 17 18:49:24 2021
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

data_wpp <- pop %>% 
  select(name) %>% 
  mutate(
    iso_a3 = countryname(name, destination = "iso3c"),
    pop_2019 = pop$`2020` * 1000,
    pop_2015 = pop$`2015` * 1000,
    pop_2010 = pop$`2010` * 1000,
    pop_2005 = pop$`2005` * 1000,
    pop_2000 = pop$`2000` * 1000,
    pop_1995 = pop$`1995` * 1000,
    pop_1990 = pop$`1990` * 1000,
    e0F = e0F$`2015-2020`,
    e0M = e0M$`2015-2020`,
    sexRatio = sexRatio$`2015-2020`,
    tfr = tfr$`2015-2020`,
  ) %>%
  filter(name != "Less developed regions, excluding China") %>% 
  select(-name)

data_wpp 

data_sf <- rnaturalearth::ne_countries(
  scale = "small", 
  type = "countries", 
  returnclass = "sf") %>% 
  select(name, iso_a3, iso_a2) %>% 
  mutate(
    continent = countrycode(iso_a3, origin = "iso3c", destination = "continent"),
    region = countrycode(iso_a3, origin = "iso3c", destination = "region"),
    name = toupper(countryname(name)),
    ) %>% 
  filter(str_detect(name, "ANTARCTICA", negate = TRUE)) %>%
  cbind(st_coordinates(st_centroid(., of_largest_polygon = TRUE))) %>% 
  rename(lon = X, lat = Y) %>% 
  left_join(., data_wpp, by = "iso_a3") %>% 
  filter(!is.na(iso_a3))

data_sf <- data_sf %>% 
  mutate(  # source Wikipedia
    pop_2019 = ifelse(iso_a3 == "GRL", 56081, pop_2019),
    pop_2019 = ifelse(iso_a3 == "FLK", 3398, pop_2019),
    pop_2015 = ifelse(iso_a3 == "FLK", 3398, pop_2015),
    )


data_sf %>% 
  as_tibble() %>% 
  print(n = Inf)


dt <- format(Sys.Date(), '%Y%m%d')
save(data_sf, file = paste0("data-raw/data_sf_", dt,".Rdata"))
usethis::use_data(data_sf, overwrite = TRUE)


# ----------------------------------------------------------------------------




