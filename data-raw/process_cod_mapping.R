# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Sun Apr  6 17:49:43 2025
# ------------------------------------------------- #
library(tidyverse)



path_map <- paste0(getwd(),"/data-raw/GBD_2021_Data_Tools_Guide/")
file_map <- "IHME_GBD_2021_A1_HIERARCHIES_Y2024M05D15.XLSX"
path_file_map <- paste0(path_map, file_map)

data_cod_mapping <- read_excel(
  path = path_file_map,
  sheet = "Cause Hierarchy") %>% 
  clean_names() %>% 
  select(
    cause_id, 
    cause_name,
    parent_id,
    parent_name,
    level,
    cause_outline,
    sort_order,
    cod_selection,
    sdg_selection,
    download
    )


usethis::use_data(data_cod_mapping, overwrite = TRUE)
