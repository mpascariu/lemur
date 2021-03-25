# --------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Thu Mar 25 14:21:22 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)
library(MortalityCauses)

# ------------------------------------------

unique(data_gbd2019_cod$region)

D <- data_gbd2019_cod %>% 
  filter(region %in% c("France", 
                       "Japan", 
                       "United Republic of Tanzania", 
                       "Mexico", 
                       "Romania",
                       "South Africa")) %>% 
  group_by(region, cause_name) %>% 
  summarise(deaths = sum(deaths)) %>% 
  mutate(perc = deaths/ sum(deaths) * 100)

rank2 <- D %>% 
  select(-deaths) %>% 
  pivot_wider(
    names_from = region,
    values_from = perc) %>% 
  rename(Tanzania = `United Republic of Tanzania`) %>% 
  arrange(desc(France)) %>% 
  print(n = Inf)

openxlsx::write.xlsx(rank2, file = "data-raw/Rank_COD_2015-19_v3.xlsx")


# ------------------------------------------
# Let's have a look at the neoplasms and cardio data in more detail

gbd_5c <- read_csv(file = "data-raw/GBD2019COD/IHME-GBD_2019_DATA-Neoplasm+Cardio_5_Countries.zip")

gbd_5c %>% 
  select(cause_id, cause_name) %>% 
  unique() %>% 
  arrange(cause_id) %>% 
  print(n = Inf)

gbd_5C <- gbd_5c %>% 
  mutate(
    cause_id2 = ifelse(cause_id < 491, "Cancer", "Cardio"),
    location_name = ifelse(location_name == "United Republic of Tanzania", "Tanzania", location_name)
    )

rank3 <- gbd_5C %>% 
  filter(cause_id2 == "Cancer",
         cause_name != "Neoplasms") %>% 
  group_by(location_name, cause_name) %>% 
  summarise(deaths = sum(val)) %>% 
  mutate(perc = deaths/ sum(deaths) * 100) %>% 
  select(-deaths) %>% 
  pivot_wider(
    names_from = location_name,
    values_from = perc) %>% 
  arrange(desc(France)) %>%
  print(n = Inf)


rank4 <- gbd_5C %>% 
  filter(cause_id2 == "Cardio",
         cause_name != "Cardiovascular diseases") %>% 
  group_by(location_name, cause_name) %>% 
  summarise(deaths = sum(val)) %>% 
  mutate(perc = deaths/ sum(deaths) * 100) %>% 
  select(-deaths) %>% 
  pivot_wider(
    names_from = location_name,
    values_from = perc) %>% 
  arrange(desc(France)) %>%
  print(n = Inf)


openxlsx::write.xlsx(rank3, file = "data-raw/Rank_Neo_2015-19.xlsx")
openxlsx::write.xlsx(rank4, file = "data-raw/Rank_Cardio_2015-19.xlsx")


