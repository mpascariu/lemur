# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed Oct 27 15:25:11 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)

library(MortalityCauses)


data_app_input


d <- data_gbd2019_sdg %>% 
  group_by(region, sex, period, cause_name, level) %>%
  summarise(deaths = sum(deaths)) %>% 
  group_by(region, sex, cause_name, level) %>% 
  mutate(sdg_change = (deaths/deaths[period == 2015] - 1) * 100) 


d %>% 
  filter(cause_name == "Interpersonal violence") %>% 
  print(n = Inf)
