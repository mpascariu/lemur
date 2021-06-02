# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed Jun 02 14:38:29 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)



data_cod_mapping <- readxl::read_xlsx(
  "data-raw/GBD_2019_Data_Tools_Guide/ICD10_Selected_CODs_Table.xlsx")


usethis::use_data(data_cod_mapping, overwrite = TRUE)
