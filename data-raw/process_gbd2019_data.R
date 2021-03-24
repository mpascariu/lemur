# --------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Wed Mar 24 12:55:19 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)
library(stringr)
library(readr)
library(janitor)

# The data have been obtained form GBD tool:
# Global Burden of Disease Collaborative Network.
# Global Burden of Disease Study 2019 (GBD 2019) Results.
# Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020.
# Available from http://ghdx.healthdata.org/gbd-results-tool.

# ------------------------------------------
# Read GBD COD files

wd        <- getwd()
path      <- paste0(getwd(),"/data-raw/GBD2019COD/")

files          <- list.files(path)
files_zip      <- grep(".zip", files, value = TRUE)
files_level2   <- grep("Level2", files, value = TRUE)
files_cardio   <- grep("-Cardio", files, value = TRUE)
files_neopl    <- grep("Neoplasms", files, value = TRUE)
path_files_zip <- paste0(path, files_zip)

# Level2 data
gbd_level2 <- paste0(path, files_level2) %>%
  # read in all the files individually, using
  # the function read_csv() from the readr package
  map(read_csv) %>% 
  # reduce with rbind into one dataframe
  reduce(rbind)  

# Cardio
gbd_cardio <- paste0(path, files_cardio) %>%
  map(read_csv) %>% 
  reduce(rbind)  

# Neoplasms
gbd_neopl <- paste0(path, files_neopl) %>%
  map(read_csv) %>% 
  reduce(rbind)  

# We could have done the read at once for all the files using 
# path_files_zip, but we want to keep separate data objects to 
# for later checks and validations.

# Bind everything here 
gbd_cod <- bind_rows(
  gbd_level2,
  gbd_cardio,
  gbd_neopl
  )

# ------------------------------------------
# Read GDB Locations Hierarchy Mapping

path_map <- paste0(getwd(),"/data-raw/GBD_2019_Data_Tools_Guide/")
file_map <- "IHME_GBD_2019_A1_HIERARCHIES_Y2020M10D15.XLSX"
path_file_map <- paste0(path_map, file_map)

cod_map <- read_excel(
  path = path_file_map, 
  sheet = "Cause Hierarchy") %>% 
  clean_names() %>% 
  filter(app_selection != "not_used")
  # The app_selection and app_selection2 columns have been added by me based on 
  # my selections


cod_map2 <- cod_map %>% 
  select(
    cause_id, 
    app_selection, 
    app_selection2)
  
  
# ------------------------------------------.
# GBD COD data wrangling


gbd <- left_join(gbd_cod, cod_map2, by = "cause_id") %>% 
  filter(
    app_selection != "not_used",
    age_name != "All Ages",
    ) %>% 
  select(
    # drop few columns
    location_name,
    sex_name,
    age_name,
    year,
    app_selection,
    app_selection2,
    val,
    upper,
    lower) %>% 
  mutate(
    # create a numerical age column
    x = str_extract(age_name, "^.{2}"),
    x = ifelse(age_name == "<1 year", 0, x),
    x = as.numeric(x)) %>% 
  select(
    # drop few more
    - age_name
  ) %>% 
  rename(
    # rename few columns to follow the life table data
    cause_name = app_selection,
    cause_name2 = app_selection2,
    region = location_name,
    sex = sex_name
  ) %>% 
  # group-by everything except 'val' and 'year' columns
  # so that we can aggregate data across years
  group_by_at(setdiff(names(.), c( "year", "val", "upper", "lower"))) %>% 
  summarise(
    deaths = sum(val),
    upper = sum(upper),
    lower = sum(lower)
    ) %>%
  # add period column in the second position 
  add_column(period = "2015-2019", .after = 1) %>% 
  # remove group_by to avoid future trouble
  ungroup()


# ------------------------------------------
# CHECK !!!

# 1. The values shown in Level2 data for Neoplasms and Cardio is the same 
# with the ones in the dedicated files
gbd_level2 %>% 
  group_by(cause_name) %>% 
  summarise(val = sum(val)) %>% 
  arrange(desc(val))

gbd_neopl %>% 
  group_by(cause_name) %>% 
  summarise(val = sum(val)) %>% 
  arrange(desc(val))

gbd_cardio %>% 
  group_by(cause_name) %>% 
  summarise(val = sum(val)) %>% 
  arrange(desc(val))

# 2. The death counts in the processed file are the same with the 
# counts in the Level2 files

gbd_level2 %>% 
  filter(age_name != "All Ages") %>% 
  select(val) %>% 
  sum()

sum(gbd$deaths)
# Since all is good and we did not miss anything out in our 
# COD selection/mapping we can move on and further process the data.

# ------------------------------------------

# Compute figures for both sexes
gbd_both <- gbd %>% 
  # group-by everything except 'deaths' and 'sex' columns
  group_by_at(setdiff(names(.), c("sex", "deaths", "upper", "lower"))) %>% 
  # aggregate data across sexes
  summarise(
    deaths = sum(deaths),
    upper = sum(upper),
    lower = sum(lower)
  ) %>%
  # add sex column in the 3rd position 
  add_column(sex = "Both", .after = 2) %>% 
  # remove group_by to avoid future trouble
  ungroup()


# Create the big dataset with 3 sexes
GBD <- bind_rows(gbd, gbd_both) %>% 
  # compute percentages of each disease for given region-period-sex and across ages
  group_by(region, period, sex) %>% 
  mutate(
    perc_deaths = deaths / sum(deaths),
    perc_upper = upper / sum(upper),
    perc_lower = lower / sum(lower),
    ) %>% 
  ungroup() %>% 
  arrange(cause_name, region, period, sex, x)

# ------------------------------------------
# CHECK POINT: 

# 3. Pick a country and see if the processing makes sense
GBD %>% 
  filter(region == "Romania") %>% 
  select(deaths:perc_lower) %>% 
  # the distributions should be equal to 3. 100% for each sex. 
  colSums()              

# 4. COMPARISON with GBD Global data 2015-2019
gbd_global <- read_csv(file = path_files_zip[6])

gbd_global$year %>% unique()
sum(gbd_global$val)
sum(gbd_level2$val)
sum(GBD$deaths)
 # We can notice a small difference between the global data and the Level2 data
 # downloaded form GBD. 

# ------------------------------------------
# include data in the package

data_gbd2019_cod <- GBD 

usethis::use_data(data_gbd2019_cod, overwrite = TRUE)

