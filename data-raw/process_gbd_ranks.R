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



IHME-GBD_DATA-Totals2019.zip

gbd_2019 <- read_csv(file = "data-raw/GBD2019/IHME-GBD_2019_DATA-5d0ba446-1.zip")
gbd_totals <- read_csv(file = "data-raw/GBD2019/IHME-GBD_DATA-Totals2019.zip")

# # Check the ages sum up to the totals
# gbd_totals %>% 
#   filter(location_name == "France") %>% 
#   group_by(year, sex_name) %>% 
#   summarise(val = sum(val))
# 
# gbd_2019 %>% 
#   filter(location_name == "France") %>% 
#   group_by(year, sex_name) %>% 
#   summarise(val = sum(val))

# ------------------------------------------

D <- data_gbd2019$data %>% 
  filter(region %in% c("FRANCE", "JAPAN", "TANZANIA", "MEXICO", "ROMANIA")) %>% 
  group_by(region, cause_name) %>% 
  summarise(deaths = sum(deaths)) %>% 
  mutate(perc = deaths/ sum(deaths) * 100)

rank1 <- D %>% 
    select(-perc) %>% 
    pivot_wider(
      names_from = region,
      values_from = deaths) %>% 
    arrange(desc(FRANCE)) %>% 
    print(n = Inf)

rank2 <- D %>% 
    select(-deaths) %>% 
    pivot_wider(
      names_from = region,
      values_from = perc) %>% 
    arrange(desc(FRANCE)) %>% 
    print(n = Inf)

openxlsx::write.xlsx(rank2, file = "data-raw/Rank_COD_2015-19.xlsx")





  
  