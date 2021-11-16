# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Tue Nov 16 14:48:49 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)



data_gbd2019_cod <- MortalityCauses::data_gbd2019_cod %>%
  select(-level) %>%
  mutate(region = toupper(region))
data_gbd2019_sdg <- MortalityCauses::data_gbd2019_sdg %>%
  select(-level) %>%
  mutate(region = toupper(region))
data_gbd2019_lt <- MortalityCauses::data_gbd2019_lt %>%
  select(-level) %>%
  mutate(region = toupper(region))

data_sf <- MortalityCauses::data_sf
data_sf$name <- toupper(data_sf$name)

data_app_input <- MortalityCauses::data_app_input
data_app_input$region <- toupper(data_app_input$region)

data_cod_mapping <- MortalityCauses::data_cod_mapping

usethis::use_data(data_gbd2019_cod, overwrite = TRUE)
usethis::use_data(data_gbd2019_sdg, overwrite = TRUE)
usethis::use_data(data_gbd2019_lt, overwrite = TRUE)
usethis::use_data(data_sf, overwrite = TRUE)
usethis::use_data(data_app_input, overwrite = TRUE)
usethis::use_data(data_cod_mapping, overwrite = TRUE)
