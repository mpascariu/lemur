# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Sun Apr  6 15:10:13 2025
# ------------------------------------------------- #

remove(list = ls())
library(tidyverse)
library(stringr)
library(readxl)
library(MortalityLaws)

# BUILD LIFE TABLES FROM PROBABILITY OF DEATH GBD DATA

# We don't used HMD or UN life table. We need compatible tables for all the 
# countries and regions in the GDB database.
# For that reason we use the GBD probability of death to generate our life
# tables. Of course these have to be within the small margin error to the officially 
# accepted life tables for the countries and regions known as having accurate 
# demographic data.

# GBD Results tool:
# SELECTION
# See the file: GBD2021_Download_Selection_Settings.xlsx
# See IHME_GBD_2019_A1_HIERARCHIES_Y2020M10D15.XLSX for a clear mapping

# ------------------------------------------
# Read GBD Probability of Death data from .zip files 
# Source: https://vizhub.healthdata.org/gbd-results/


wd        <- getwd()
path      <- paste0(getwd(),"/data-raw/IHME_GBD2021_Data/PoD/")
files     <- list.files(path)
files_zip <- grep(".zip", files, value = TRUE)
path_files_zip <- paste0(path, files_zip)

data <- path_files_zip %>% 
   # read in all the files individually, using
   # the function read_csv() from the readr package
  map(read_csv) %>% 
   # reduce with rbind into one dataframe
  reduce(rbind)  

year_selection <- c(1990, 1995, 2000, 2005, 2010, 2015, 2019, 2020, 2021)

# ------------------------------------------
# Read GDB Locations Hierarchy Mapping

path_map <- paste0(getwd(),"/data-raw/GBD_2021_Data_Tools_Guide/")
file_map <- "IHME_GBD_2021_A1_HIERARCHIES_Y2024M05D15.XLSX"
path_file_map <- paste0(path_map, file_map)


region_map <- read_excel(
  path = path_file_map,
  sheet = "GBD 2021 Locations Hierarchy") %>% 
  clean_names() %>% 
  select(location_id, location_name, level) %>% 
  rename(location_level = level)

# ------------------------------------------
# GBD Probability of death data (qxn)

gbd_qxn <- data %>% 
  select(
    # keep relevant columns
    location_id,
    year,
    sex_name,
    age_name,
    measure_name,
    val) %>% 
  # add the region names from region_map based on location ID
  left_join(region_map, by = "location_id") %>% 
  # Capitalize the macro-regions
  mutate(location_name = ifelse(location_level == 3, location_name, toupper(location_name))) %>%
  filter(
    measure_name == "Probability of death",
    year %in% year_selection,
    age_name != "All ages"
    ) %>% 
  mutate(
    x = ifelse(age_name == "<1 year", 0, age_name),
    x = ifelse(x == "12-23 months", 1, x),
    x = ifelse(x == "95+ years", 95, x),
    x = str_remove(x, "-.*"),
    x = as.numeric(x),
    sex_name = tolower(sex_name)
  ) %>% 
  rename(
    # rename few columns to follow the life table data
    region = location_name,
    sex = sex_name,
    period = year,
    qxn = val
  ) %>% 
  select(region, period, sex, x, qxn) %>% 
  arrange(region, period, sex, x)





# ------------------------------------------
# BUILD complete abridged life tables with {MortalityLaws}

# we will have to create 6750 life tables covering the 0-110 age range
# Because we have data on the 0-95 age range we will need to have an intermediary 
# step on which we extend the death probability vector up to age 110 using the 
# Kannisto model.

cases <- gbd_qxn %>% 
  select(-x, -qxn) %>% 
  unique()

n <- nrow(cases)

x_fitted    = seq(55, 80, by = 5)
x_predicted = seq(55, 110, by = 5)
x_full      = c(0:2, seq(5,110, by=5))

i = 1

LTS <- NULL
for(i in 1:n) {
  # selection
  S <- unlist(cases[i, ])
  print(paste0(i, "/", n, " - ", paste(S, collapse = "-")))
  
  # selected data
  d <- gbd_qxn %>% 
    filter(region == S[1],
           period == S[2], 
           sex    == S[3]
           ) 
  
  # The Kannisto model
  qx_fitted    <- unlist(d[d$x %in% x_fitted, "qxn"])
  qx_predicted <- MortalityLaw(
    x = x_fitted, 
    qx = qx_fitted, 
    law = 'kannisto', 
    opt.method = 'LF2') %>% 
    predict(., x = x_predicted)
  
  qx_full = c(unlist(d[d$x <= 50, "qxn"]), qx_predicted)
  
  # Life table function
  lt <- LifeTable(
    x = x_full,
    qx = qx_full,
    sex = ifelse(S[3] == "both", "total", S[3])
    )$lt %>% 
    # add identification info
    add_column(
      region = S[1],
      period = S[2], 
      sex    = S[3],
      .before = 1) 
 
  LTS <- rbind(LTS, lt) 
}


data_gbd2021_lt <- as_tibble(LTS) %>%
  mutate(
    region = factor(region)
  ) 

# CHECK if there are any NAs in the life tables. There should be none!
data_gbd2021_lt %>% 
  filter(is.na(mx)) %>% 
  print(n = Inf)



# ------------------------------------------
# include data in the package
dt <- format(Sys.Date(), '%Y%m%d')
save(data_gbd2021_lt, file = paste0("data-raw/IHME_GBD2021_Data/data_gbd2021_lt_", dt,".Rdata"))
usethis::use_data(data_gbd2021_lt, overwrite = TRUE)




