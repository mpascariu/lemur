# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# License: GNU General Public License v3.0
# Last update: Sun Mar 14 19:15:54 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)
library(readxl)

# Load abridged life tables downloaded from United Nation - 
# Department of Economic and Social Affairs Population Dynamics  
# url: https://population.un.org/wpp/Download/Standard/Mortality/
# (see bottom of the webpage).

LT_both <- read_xlsx(
  path = "data-raw/WPP2019LT/WPP2019_MORT_F17_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx",
  skip = 16
  ) %>% 
  add_column(Sex = "Both", .after = 1)

LT_male <- read_xlsx(
  path = "data-raw/WPP2019LT/WPP2019_MORT_F17_2_ABRIDGED_LIFE_TABLE_MALE.xlsx",
  skip = 16
  ) %>% 
  add_column(Sex = "Male", .after = 1)

LT_female <- read_xlsx(
  path = "data-raw/WPP2019LT/WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.xlsx",
  skip = 16
  ) %>% 
  add_column(Sex = "Female", .after = 1)


# bind the 3 datasets
LT <- bind_rows(LT_both,
                LT_male,
                LT_female) %>% 
  filter(Period == "2015-2020")

LT

# rename columns
col_names <- c("index",
               "sex",
               "variant",
               "region",
               "note",
               "country_code",
               "type",
               "parent_code",
               "period",
               "x",
               "n",
               "mxn",
               "qxn",
               "pxn",
               "lx",
               "dxn",
               "Lxn",
               "Sxn",
               "Tx",
               "ex",
               "axn")

colnames(LT) <- col_names

# format numerical values and replace "..." with zeros
foo <- function(x) {
  x[x == "..."] <- 0
  as.numeric(x)
}

LTs <- LT %>% 
  mutate(across(col_names[10:21], ~foo(.)))

# # export a sample in excel to check if all is alright
# openxlsx::write.xlsx(LTs[1:1e4, ], "data-raw/LT_sample.xlsx")

# Meta data
meta <- tibble(`Column Name` = col_names,
               Description = colnames(LT_both))

data_wpp2019_lt <- list(data = LTs,
                        meta = meta)

# include data in the package

usethis::use_data(data_wpp2019_lt, overwrite = TRUE)



