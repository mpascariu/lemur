# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed Nov 17 19:56:02 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)

load("data-raw/data_gbd2019_cod_20211117.Rdata")
load("data-raw/data_gbd2019_sdg_20211117.Rdata")

D <- data_gbd2019_cod

data_app_input <- list(
  region     = levels(D$region),
  cause_name = levels(D$cause_name),
  period = unique(D$period),
  sex    = unique(D$sex),
  x      = unique(D$x)
)


data_app_input$cause_name_sdg <- levels(data_gbd2019_sdg$cause_name)


usethis::use_data(data_app_input, overwrite = TRUE)
