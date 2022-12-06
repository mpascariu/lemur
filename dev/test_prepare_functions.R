# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Tue Nov 16 14:05:38 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)
library(dttest)
library(lemur)


region1 = "ROMANIA"
region2 = "MEXICO"
gender = "male"
gender2 = "female"

cod <- data_gbd2019_cod %>%
  filter(period == 2000,
         sex == "male")
sdg <- data_gbd2019_sdg %>%
  filter(period == 2000,
         sex == "male")
lt  <- data_gbd2019_lt %>%
  filter(period == 2000,
         sex == "male")

M <- build_reduction_matrix(
  data = cod,
  select_cod = unique(cod$cause_name),
  select_x = 0:110,
  cod_change = -50
)

prepare_data_mode_cod(
  cod = cod,
  lt = lt,
  region1 = region1,
  cod_change = M
)

prepare_data_mode_cntr(
  cod = cod,
  lt = lt,
  region1 = region1,
  region2 = region2,
  cod_change = M
)

prepare_data_mode_sex(
  cod = cod,
  lt = lt,
  region1 = region1,
  cod_change = M
)

prepare_data_mode_sdg(
  cod = sdg,
  lt = lt,
  region = region2,
  sdg_1 = 0,
  sdg_3 = -50,
  sdg_4 = -20,
  sdg_5 = -20,
  sdg_6 = -10,
  sdg_7 = -10
)

lemur::run_app()


