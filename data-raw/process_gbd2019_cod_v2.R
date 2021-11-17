# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed Nov 17 19:11:36 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)
library(stringr)
library(readr)
library(readxl)
library(janitor)
library(ungroup)
library(countrycode)

# # The data have been obtained form GBD tool:
# # Global Burden of Disease Collaborative Network.
# # Global Burden of Disease Study 2019 (GBD 2019) Results.
# # Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020.
# # Available from http://ghdx.healthdata.org/gbd-results-tool.
#  DATE OF DOWNLOAD: 13-14 October 2021

# SELECTION
#  Base: Single
#  Location: select only countries and territories (214)
#  Year: 1990, 1995, 2000, 2005, 2010, 2015, 2019 (selected 1 at the time)
#  Context: Cause (1)
#  
#  Age: 
#  <1year, 
#  1 to 4,  
#  5 to 9,  
#  10 to 14,  
#  15 to 19,  
#  20 to 24,  
#  25 to 29,  
#  30 to 34,  
#  35 to 39,  
#  40 to 44,  
#  45 to 49,  
#  50 to 54,  
#  55 to 59,  
#  60 to 64,  
#  65 to 69,  
#  70 to 74,  
#  75 to 79,  
#  80 to 84,  
#  85 to 89,  
#  90 to 94,  
#  95 plus. (21)
# 
# Metric: Number (1)
# Measure: Deaths (1)
# Sex: Male, Female (2)  
# Cause: select only level 2 causes

## Level 3 causes selection
# Tuberculosis
# Malaria
# Maternal disorders
# Neonatal disorders
# Colon and rectum cancer
# Tracheal, bronchus, and lung cancer
# Rheumatic heart disease
# Ischemic heart disease
# Stroke
# Diabetes mellitus
# Poisonings
# Exposure to forces of nature
# Self-harm

# See IHME_GBD_2019_A1_HIERARCHIES_Y2020M10D15.XLSX for a clear mapping
# Downloading all data at one from GBD won't work. That's why we have to download 
# year by year and level 2 and level 3 cods selection separately.


# # ------------------------------------------
# # Read GBD COD files
# 
# wd        <- getwd()
# path      <- paste0(getwd(),"/data-raw/GBD2019COD_20211013/")
# 
# files          <- list.files(path)
# files_zip      <- grep(".zip", files, value = TRUE)
# files_level2   <- grep("_L2_", files, value = TRUE)
# files_level3   <- grep("_L3_Selection_", files, value = TRUE)
# path_files_zip <- paste0(path, files_zip)
# 
# # Level2 data
# gbd_level2 <- paste0(path, files_level2) %>%
#   # read in all the files individually, using
#   # the function read_csv() from the readr package
#   map(read_csv) %>%
#   # reduce with rbind into one dataframe
#   reduce(rbind)
# 
# # Level3 selected data
# gbd_level3 <- paste0(path, files_level3) %>%
#   map(read_csv) %>%
#   reduce(rbind)
# 
# 
# # We could have done the read at once for all the files using
# # path_files_zip, but we want to keep separate data objects
# # for later checks and validations.
# 
# 
# save(
#   gbd_level2,
#   gbd_level3,
#   file = "data-raw/GBD_COD_Input_20211013.Rdata"
# )



# Load all datasets created above
load("data-raw/GBD_COD_Input_20211013.Rdata")

# Bind everything here
gbd_cod <- bind_rows(
  gbd_level2,
  gbd_level3
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
  filter(cod_selection != "no") %>% 
  select(
    cause_id, 
    cod_selection)



# ------------------------------------------.
# GBD COD data wrangling

gbd <- left_join(gbd_cod, cod_map, by = "cause_id") %>% 
  filter(
    cod_selection != "no",
    age_name != "All Ages",
  ) %>% 
  select(
    # drop few columns
    location_name,
    sex_name,
    age_name,
    year,
    cod_selection,
    val,
    upper,
    lower) %>% 
  mutate(
    # create a numerical age column
    x = str_extract(age_name, "^.{2}"),
    x = ifelse(age_name == "<1 year", 0, x),
    x = as.numeric(x),
    # use only lower-case characters
    sex_name = tolower(sex_name)) %>% 
  select(
    # drop few more
    - age_name
  ) %>% 
  rename(
    # rename few columns to follow the life table data
    cause_name = cod_selection,
    region = location_name,
    sex = sex_name,
    period = year,
    median = val
  ) %>%
  pivot_longer(cols = median:lower, names_to = "level", values_to = "deaths") %>% 
  # group-by everything except 'val' and 'year' columns
  # so that we can aggregate data across years
  group_by_at(setdiff(names(.), c("deaths"))) %>% 
  summarise(deaths = sum(deaths)) %>%
  # We have included level 3 categories in cardio and neoplasms. These have to 
  # be extracted from 'Other cardiovascular' and 'Other neoplasms' to
  # avoid double counting.
  ungroup() %>% 
  filter(level == "median") %>% 
  complete(x, nesting(region, sex, period, cause_name, level)) %>% 
  mutate(deaths = replace_na(deaths, 0)) %>% 
  group_by(region, sex, period, x, level) %>% 
  mutate(
    deaths = ifelse(cause_name == 'Other Cardiovascular', deaths - deaths[cause_name == 'Ischemic Heart Disease'] - deaths[cause_name == 'Stroke'], deaths), 
    deaths = ifelse(cause_name == 'Other Neoplasms', deaths - deaths[cause_name == 'Colon and Rectum Cancer'] - deaths[cause_name == 'Lung Cancer'], deaths),
    deaths = pmax(deaths, 0) # for some reason we get negative values as well!!!
  ) %>% 
  # remove group_by to avoid future trouble
  ungroup()


# ------------------------------------------
# CHECK POINT !!!

# 1. The death counts in the processed file are the same with the 
# counts in the Level2 files

gbd_level2 %>% 
  filter(age_name != "All Ages") %>% 
  select(val) %>% 
  sum()

gbd %>% 
  filter(level == "median") %>% 
  select(deaths) %>% 
  sum()

# Since all is good and we did not miss anything out in our 
# COD selection/mapping we can move on and further process the data.



# ------------------------------------------
# UNGROUP the last age interval (95+)

# THIS IS A FUNCTION
#' Ungroup the values of the last age interval (95+) using the 
#' pclm method by adding 3 additional age bands (100, 105, and 110).
#' 
#' We do not extend the values towards the younger ages if they are missing
#' but consider them equal to zero. 
#' @param X A tibble containing COD data
ungroup_last_age_int <- function(X) {
  options(dplyr.summarise.inform = FALSE)
  
  out <- NULL
  n   <- nrow(X)
  
  if (n != 0 & sum(X$deaths) > 1) {
    # The input data must have few rows to be able to do anything
    cols <- c("region", 
              "period",
              "sex",
              "cause_name", 
              "level")
    p   <- X$deaths[n]/sum(X$deaths) 
    
    if (X$x[n] == 95 & p >= 0.01) {
      # We are doing the ungrouping only if the last age interval is 95+
      # AND 
      # the death count in the group is higher than 1% of the total. 
      # Else return the input.
      
      X$deaths[X$deaths == 0] <- 0.0001
      M <- pclm(x = X$x,
                y = X$deaths,
                out.step = 1,
                nlast = 16)
      
      # Ages
      ages <- sort(c(rep(seq(X$x[1], 105, by = 5), 5), 110))
      # Create the output table in the same format as the input table + 
      # the additional ages
      
      out <- tibble(X[1, cols],
                    x = ages,
                    deaths = M$fitted) %>% 
        # group-by everything except 'deaths'
        group_by_at(setdiff(names(.), "deaths")) %>%
        summarise(deaths = sum(deaths)) %>% 
        ungroup()
      
    } else {
      out <- X[, c(cols, "x", "deaths")]
      
    }
  }
  return(out)
}

# Since we have the ungrouping function ready 
# we can prepare the data
key <- c("region", "period", "sex", "cause_name", "level")
cases <- gbd %>% 
  filter(
    cause_name != "Neonatal Disorders",
    ) %>% 
  select(region, period, sex, cause_name, level) %>% 
  unique() %>% 
  # add a key for faster filtering in the loop
  unite(col = "key", 
        all_of(key), 
        sep = "-",
        remove = TRUE) %>% 
  unlist() %>% 
  as.character()

threshold = 75 # threshold age

gbd2 <- gbd %>% 
  filter(
    x >= threshold
  ) %>%  # use only data above threshold for ungrouping
  unite("key", key, sep = "-", remove = FALSE) 


system.time({ 
  # THIS LOOP SHOULD TAKE SOME TIME!! Be prepared or go grab a coffee.
  gbd_to_110 <- NULL
  i = 1
  j = cases[i]
  lc = length(cases)
  for (j in cases) {
    print(paste0(i,"/", lc, "- ", j))
    gbd_to_110 <- gbd2 %>% 
      filter(key == j) %>% 
      ungroup_last_age_int(.) %>% 
      bind_rows(gbd_to_110, .)
    i = i + 1
  }
})



gbd110 <- gbd %>% 
  filter(x < threshold) %>% 
  bind_rows(., gbd_to_110) %>% 
  arrange(level, cause_name, region, period, sex, x)

# UNGROUPING FINISHED HERE

# ------------------------------------------
# Compute figures for "both" sexes combined
gbd_both <- gbd110 %>% 
  # group-by everything except 'deaths' and 'sex' columns
  group_by_at(setdiff(names(.), c("sex", "deaths"))) %>% 
  # aggregate data across sexes
  summarise(deaths = sum(deaths)) %>%
  # add sex column in the 3rd position 
  add_column(sex = "both", .after = 2) %>% 
  # remove group_by to avoid future trouble
  ungroup() 


# Create the big dataset with 3 sexes
GBD <- bind_rows(gbd110, gbd_both) %>% 
  complete(x, nesting(region, sex, period, cause_name, level)) %>% 
  mutate(deaths = replace_na(deaths, 0))

# ------------------------------------------
# CHECK POINT: 


sum(gbd_level2$val)
GBD %>% 
  filter(level == "median") %>% 
  select(deaths) %>% 
  sum()/2
# We can notice a small difference between the global data and the Level2 data
# downloaded form GBD. However, this is a data provider issue.
# 
# We can notice a small difference between Level2 data and processed data
# due to ungrouping residuals.

# ------------------------------------------
# include data in the package
rank <- GBD %>%
  filter(sex == "both",
         level == "median") %>% 
  group_by(cause_name) %>% 
  summarise(value = sum(deaths)) %>% 
  arrange(value)


# data_gbd2019_cod <- data_gbd2019_cod %>%
#   filter(level == "median") %>%
#   mutate(
#     cause_name = factor(cause_name, levels = rank$cause_name),
#     region = factor(region)
#     )

data_gbd2019_cod <- GBD %>%
  filter(level == "median") %>% 
  mutate(
    cause_name = factor(cause_name, levels = rank$cause_name),
    region = toupper(countryname(region)),
    region = factor(region)
  ) %>% 
  drop_na() %>% 
  select(-level) 


dt <- format(Sys.Date(), '%Y%m%d')
save(data_gbd2019_cod, file = paste0("data-raw/data_gbd2019_cod_", dt,".Rdata"))
usethis::use_data(data_gbd2019_cod, overwrite = TRUE)

# ----------------------------------------------------------------------------






