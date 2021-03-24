# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# License: GNU General Public License v3.0
# Last update: Sat Mar 20 12:19:03 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)

library(MortalityCauses )


data_gbd2019$data %>% 
  select(cause_name) %>% 
  unique() %>% 
  print(n = Inf)


# ------------------------------------------

D <- data_gbd2019$data %>% 
  filter(region %in% c("FRANCE", "JAPAN", "TANZANIA", "MEXICO", "ROMANIA")) %>% 
  group_by(region, cause_name) %>% 
  summarise(deaths = sum(deaths)) %>% 
  mutate(perc = deaths/ sum(deaths) * 100)

# rank1 <- D %>% 
#     select(-perc) %>% 
#     pivot_wider(
#       names_from = region,
#       values_from = deaths) %>% 
#     arrange(desc(FRANCE)) %>% 
#     print(n = Inf)

rank2 <- D %>% 
    select(-deaths) %>% 
    pivot_wider(
      names_from = region,
      values_from = perc) %>% 
    arrange(desc(FRANCE)) %>% 
    print(n = Inf)

openxlsx::write.xlsx(rank2, file = "data-raw/Rank_COD_2015-19.xlsx")


# ------------------------------------------
# Let's have a look at the neoplasms and cardio data in more detail

gbd_5c <- read_csv(file = "data-raw/GBD2019/IHME-GBD_2019_DATA-Neoplasm+Cardio_5_Countries.zip")

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


