# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Thu Jun 10 16:04:10 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)
library(stringr)
library(readr)
library(readxl)
library(janitor)
library(ungroup)

# # The data have been obtained form GBD tool:
# # Global Burden of Disease Collaborative Network.
# # Global Burden of Disease Study 2019 (GBD 2019) Results.
# # Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020.
# # Available from http://ghdx.healthdata.org/gbd-results-tool.
# 
# # ------------------------------------------
# # Read GBD COD files
# 
# # wd        <- getwd()
# # path      <- paste0(getwd(),"/data-raw/GBD2019COD/")
# # 
# # files          <- list.files(path)
# # files_zip      <- grep(".zip", files, value = TRUE)
# # files_level2   <- grep("Level2", files, value = TRUE)
# # files_cardio   <- grep("-Cardio", files, value = TRUE)
# # files_neopl    <- grep("Neoplasms", files, value = TRUE)
# # path_files_zip <- paste0(path, files_zip)
# # 
# # # Level2 data
# # gbd_level2 <- paste0(path, files_level2) %>%
# #   # read in all the files individually, using
# #   # the function read_csv() from the readr package
# #   map(read_csv) %>% 
# #   # reduce with rbind into one dataframe
# #   reduce(rbind)  
# # 
# # # Cardio
# # gbd_cardio <- paste0(path, files_cardio) %>%
# #   map(read_csv) %>% 
# #   reduce(rbind)  
# # 
# # # Neoplasms
# # gbd_neopl <- paste0(path, files_neopl) %>%
# #   map(read_csv) %>% 
# #   reduce(rbind)  
# # 
# # # We could have done the read at once for all the files using 
# # # path_files_zip, but we want to keep separate data objects
# # # for later checks and validations.
# # 
# # 
# # save(
# #   gbd_level2,
# #   gbd_cardio,
# #   gbd_neopl,
# #   file = "data-raw/GBD_COD_Input.Rdata"
# # )
# # 

# Load all datasets created above
load("data-raw/GBD_COD_Input.Rdata")

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
  # The app_selection and app_selection2 columns have been added by us based on
  # our selections

cod_map2 <- cod_map %>% 
  select(
    cause_id, 
    app_selection)
  
  
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
    cause_name = app_selection,
    region = location_name,
    sex = sex_name
  ) %>% 
  # group-by everything except 'val' and 'year' columns
  # so that we can aggregate data across years
  group_by_at(setdiff(names(.), c( "year", "val", "upper", "lower"))) %>% 
  summarise(
    median = sum(val),
    upper = sum(upper),
    lower = sum(lower)
    ) %>%
  # add period column in the second position 
  add_column(period = "2015-2019", .after = 1) %>% 
  # remove group_by to avoid future trouble
  ungroup() %>% 
  pivot_longer(cols = median:lower, names_to = "level", values_to = "deaths") 


# ------------------------------------------
# CHECK POINT !!!

# 1. The values shown in Level2 data for Neoplasms and Cardio are the same 
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
  
  if (n != 0) {
    # The input data must have few rows to be able to do anything
    cols <- c("region", 
              "period",
              "sex",
              "cause_name", 
              "level")
    p   <- X$deaths[n]/sum(X$deaths) 
    
    if (X$x[n] == 95 & p >= 0.005) {
      # We are doing the ungrouping only if the last age interval is 95+
      # AND the death count in the group is higher than 0.5% of the total. 
      # Else return the input.
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
  select(region, period, sex, cause_name, level) %>% 
  unique() %>% 
   # add a key for faster filtering in the loop
  unite(col = "key", 
        all_of(key), 
        sep = "-",
        remove = TRUE) %>% 
  unlist() %>% 
  as.character()

threshold = 75

gbd2 <- gbd %>% 
  filter(
    x >= threshold
    ) %>%  # use only data above 70 for ungrouping
  unite("key", key, sep = "-", remove = FALSE) 



system.time({ 
  # THIS LOOP SHOULD TAKE SOME MINUTES!!! Be prepared or go grab a coffee.
  #  15m30s on my maybe 8yo Thinkpad T530. 
  #  Yes, I am still using this bad boy. It won't die. 
  gbd_to_110 <- NULL
  i = 1
  j = cases[i]
  lc = length(cases)
  for (j in cases) {
    # print(paste0(i,"/", lc, "- ", j))
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
# Compute figures for both sexes combined
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
  # compute percentages of each disease for
  # given age-region-period-sex and across ages
  group_by(region, period, sex, x, level) %>%
  # group_by(region, period, sex, level, x, cause_name) %>%
  mutate(perc = deaths / sum(deaths)) %>%
  ungroup() 

# ------------------------------------------
# CHECK POINT: 

# 3. Pick a country and see if the processing makes sense
# the distributions for each age group must be 100% in each scenario. OK.
GBD %>% 
  filter(region == "Romania") %>%
  group_by(sex, x, level) %>% 
  summarise(perc = sum(perc))

# the distributions for each cod should be different than 100%. OK.
GBD %>% 
  filter(region == "Romania") %>%
  group_by(sex, cause_name, level) %>% 
  summarise(perc = sum(perc))

# The sum of all % in region-period-sex-level must be equal to the 
# no. of age groups
length(unique(GBD$x))  # 24

GBD %>% 
  filter(region == "Romania") %>%
  group_by(region, period, sex, level) %>% 
  summarise(perc = sum(perc))



# # 4. COMPARISON with GBD Global data 2015-2019
# gbd_global <- read_csv(file = path_files_zip[6])
# gbd_global$year %>% unique()
# sum(gbd_global$val)

sum(gbd_level2$val)
GBD %>% 
  filter(level == "median") %>% 
  select(deaths) %>% 
  sum()
 # We can notice a small difference between the global data and the Level2 data
 # downloaded form GBD. However, this is a data provider issue.
 # 
 # We can notice a small difference between Level2 data and processed data
 # due to ungrouping residuals (116 out of 5B)

# ------------------------------------------
# include data in the package
rank <- data_gbd2019_cod %>%
  filter(region == "Romania",
         sex == "both",
         level == "median") %>% 
  group_by(cause_name) %>% 
  summarise(value = sum(deaths)) %>% 
  arrange(value)

data_gbd2019_cod <- GBD %>%
  mutate(cause_name = factor(cause_name, levels = rank$cause_name)) %>% 
  select(-perc)

usethis::use_data(data_gbd2019_cod, overwrite = TRUE)

