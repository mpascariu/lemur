# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed Nov 17 19:11:24 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)
library(stringr)
library(readxl)
library(MortalityLaws)
library(countrycode)

# ------------------------------------------
# Read GBD LIFE TABLE files
# Source: http://ghdx.healthdata.org/record/ihme-data/gbd-2019-life-tables-1950-2019

wd        <- getwd()
path      <- paste0(getwd(),"/data-raw/GBD2019LT/")
files     <- list.files(path)
files_zip <- grep(".zip", files, value = TRUE)
path_files_zip <- paste0(path, files_zip)

data <- path_files_zip %>% 
   # read in all the files individually, using
   # the function read_csv() from the readr package
  map(read_csv) %>% 
   # reduce with rbind into one dataframe
  reduce(rbind)  


# ------------------------------------------
# Read GDB Locations Hierarchy Mapping

path_map <- paste0(getwd(),"/data-raw/GBD_2019_Data_Tools_Guide/")
file_map <- "IHME_GBD_2019_A1_HIERARCHIES_Y2020M10D15.XLSX"
path_file_map <- paste0(path_map, file_map)

location_map <- read_excel(
  path = path_file_map, 
  sheet = "GBD 2019 Locations Hierarchy") %>% 
  # for now we will be working with the 204 countries only 
  filter(Level == 3)

# ------------------------------------------
# GBD Probability of death data (qxn)

year_selection <- c(1990, 1995, 2000, 2005, 2010, 2015, 2019)


gbd_qxn <- data %>% 
  select(
    # keep relevant columns
    location_name,
    year_id,
    sex_name,
    age_group_name,
    measure_name,
    val,
    upper,
    lower) %>% 
  filter(
    year_id %in% year_selection,
    measure_name == "Probability of death no-shock with hiv",
    location_name %in% location_map$`Location Name`,
    ) %>% 
  mutate(
    x = word(age_group_name, 1),
    x = ifelse(x == "<1", 0, x),
    x = as.numeric(x),
    level = "median") %>% 
  select(
    # drop few more
    - age_group_name,
    - measure_name,
    - lower,
    - upper,
  ) %>% 
  rename(
    # rename few columns to follow the life table data
    region = location_name,
    sex = sex_name,
    period = year_id,
    qxn = val
  ) %>% 
  arrange(level, region, period, sex, x)


# ------------------------------------------
# BUILD complete abridged life tables with {MortalityLaws}

# we will have to create 1836 life tables
# cases <- gbd_qxn_avg %>% 
cases <- gbd_qxn %>% 
  select(-x, -qxn) %>% 
  unique()

ages <- unique(gbd_qxn$x)
n    <- nrow(cases)

LTS <- NULL
for(i in 1:n) {
  # selection
  S <- unlist(cases[i, ])
  print(paste0(i, "/", n, " - ", paste(S, collapse = "-")))
  
  # selected data
  d <- gbd_qxn %>% 
    filter(region == S[1],
           period == S[2], 
           sex    == S[3],
           level  == S[4]) 
  
  # Life table function
  lt <- LifeTable(
    x = ages,
    qx = d$qxn,
    sex = ifelse(S[3] == "both", "total", S[3])
    )$lt %>% 
    # add identification info
    add_column(
      region = S[1],
      period = S[2], 
      sex    = S[3],
      level  = S[4],
      .before = 1) 
 
  LTS <- rbind(LTS, lt) 
}


data_gbd2019_lt <- as_tibble(LTS) %>%
  mutate(
    region = toupper(countryname(region)),
    region = factor(region)
  ) %>% 
  drop_na() %>% 
  select(-level) 

# ------------------------------------------
# include data in the package
dt <- format(Sys.Date(), '%Y%m%d')
save(data_gbd2019_cod, file = paste0("data-raw/data_gbd2019_lt_", dt,".Rdata"))
usethis::use_data(data_gbd2019_lt, overwrite = TRUE)




