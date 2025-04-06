# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Sun Apr  6 07:29:37 2025
# ------------------------------------------------- #

remove(list = ls())
library(tidyverse)
library(stringr)
library(readr)
library(readxl)
library(janitor)
library(ungroup)

# This script is similar with the "process_gbd2021_cod.R".
# Here we are creating a similar dataset but following a different
# groupping based on the US's SDG goals. This means we have to include 
# more level 3 COD's.
# 
# ------------------------------------------
# Read GDB Locations Hierarchy Mapping

path_map <- paste0(getwd(),"/data-raw/GBD_2021_Data_Tools_Guide/")
file_map <- "IHME_GBD_2021_A1_HIERARCHIES_Y2024M05D15.XLSX"
path_file_map <- paste0(path_map, file_map)

cod_map <- read_excel(
  path = path_file_map,
  sheet = "Cause Hierarchy") %>%
  clean_names() %>%
  filter(sdg_selection != "no") %>% 
  select(
    cause_id, 
    sdg_selection)

# For some reason several regions have different names between level2 and 
# level3 data. We need a mapping for this in order to harmonize the datasets.  
region_map <- read_excel(
  path = path_file_map,
  sheet = "GBD 2021 Locations Hierarchy") %>% 
  clean_names() %>% 
  select(location_id, location_name, level) %>% 
  rename(location_level = level)

# ------------------------------------------
# Read GBD COD files

path           <- paste0(getwd(),"/data-raw/IHME_GBD2021_Data")
files          <- list.files(path, full.names = TRUE, recursive = TRUE)
files_zip      <- grep(".zip", files, value = TRUE)
files_level2   <- grep("CoD_Level_2", files, value = TRUE)
files_level3   <- grep("CoD_Level_3", files, value = TRUE)



# Level2 data
gbd_level2 <- files_level2 %>%
  # read in all the files individually, using
  # the function read_csv() from the readr package
  map(read_csv) %>%
  # reduce with rbind into one dataframe
  reduce(rbind) %>% 
  # add the region names from region_map based on location ID
  select(-location_name) %>% 
  left_join(region_map, by = "location_id")

# Level3 selected data
gbd_level3 <- files_level3 %>%
  map(read_csv) %>%
  reduce(rbind) %>% 
  # Select only the groups we want to keep the data on. Since the level2 data 
  # already contains all the death count, the groups below will need to be 
  # subtract from the parent level 2 counts.
  filter(cause_name %in% cod_map$sdg_selection) %>% 
  # add the region names from region_map based on location ID
  select(-location_name) %>% 
  left_join(region_map, by = "location_id")


# Bind everything here
gbd_cod <- bind_rows(
  gbd_level2,
  gbd_level3,
)

# ------------------------------------------.
# GBD COD data wrangling

gbd <- left_join(gbd_cod, cod_map, by = "cause_id") %>% 
  filter(
    sdg_selection != "no",
    age_name != "All ages",
  ) %>% 
  select(
    # drop few columns
    location_name,
    location_id,
    location_level,
    sex_name,
    age_name,
    year,
    sdg_selection,
    val) %>% 
  # Capitalize the macro-regions
  mutate(location_name = ifelse(location_level == 3, location_name, toupper(location_name))) %>%
  # create a numerical age column
  mutate(
    x = ifelse(age_name == "<1 year", 0, age_name),
    x = ifelse(x == "12-23 months", 1, x),
    x = ifelse(x == "95+ years", 95, x),
    x = str_remove(x, "-.*"),
    x = as.numeric(x),
    # use only lower-case characters
    sex_name = tolower(sex_name)) %>% 
  select(
    # drop few more
    - age_name,
    - location_id,
    - location_level,
  ) %>% 
  rename(
    # rename few columns to follow the life table data
    cause_name = sdg_selection,
    region = location_name,
    sex = sex_name,
    period = year,
    deaths = val
  ) %>%
  # group-by everything except 'val' and 'year' columns
  # so that we can aggregate data across years following the mapping
  filter(cause_name != "Maternal and neonatal disorders")  %>%
  group_by_at(setdiff(names(.), c("deaths"))) %>%
  summarise(deaths = sum(deaths)) %>%
  pivot_wider(names_from = cause_name, values_from = deaths) %>% 
  pivot_longer(cols = -c(1:4), names_to = "cause_name", values_to = "deaths") %>%  
  ungroup() %>% 
  # complete(x, nesting(region, sex, period, cause_name)) %>% 
  mutate(deaths = replace_na(deaths, 0)) %>% 
  # We have included 8 level_3 categories. These have to
  # be extracted from level_2 totals to avoid double counting.
  group_by(x, region, sex, period) %>%
  mutate(
    deaths = ifelse(cause_name == 'Respiratory Infections (excl. Tuberculosis)', deaths - deaths[cause_name == 'Tuberculosis'], deaths),
    deaths = ifelse(cause_name == 'Neglected Tropical Diseases (excl. Malaria)', deaths - deaths[cause_name == 'Malaria'], deaths),
    deaths = ifelse(cause_name == 'Kidney disease (excl. Diabetes)', deaths - deaths[cause_name == 'Diabetes mellitus'], deaths),
    deaths = ifelse(cause_name == 'Injuries (excl. Poisonings)', deaths - deaths[cause_name == 'Poisonings'] - deaths[cause_name == 'Exposure to forces of nature'], deaths),
    deaths = ifelse(cause_name == 'Interpersonal Violence', deaths - deaths[cause_name == 'Self-harm'], deaths),
  ) %>%
  # remove group_by to avoid future trouble
  ungroup()   


# ------------------------------------------
# CHECK POINT !!!

# check if there are no NAs in the data
gbd[!complete.cases(gbd),]

# CHECK POINT
# Reasoning: In level 2 data we have the complete death counts. However, we are 
# looking and using sub-groups in from level 3 that we append to the data set
# and subtract from level 2 totals. Here we check that after the subtracting 
# operation we end up with the same number of deaths (not more not less).
# 
# X2 <- gbd_level2 %>%
#   filter(sex_name == "Female",
#          year == 2021,
#          age_name == "All ages",
#          cause_name == "All causes"
#   ) %>%
#   select(location_name, val) %>%
#   rename(region = location_name,
#          val2 = val)
# 
# 
# dt <- gbd %>%
#   filter(sex == "female", period == 2021) %>%
#   group_by(region) %>%
#   summarise(deaths = sum(deaths)) %>%
#   ungroup() %>%
#   left_join(X2, by = "region") %>%
#   mutate(
#     delta = round(val2 - deaths, 6)
#   )
# 
# dt %>% print(n = Inf)


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
              "cause_name")
    p   <- X$deaths[n]/sum(X$deaths)
    
    if (X$x[n] == 95 & X$deaths[n] > 2){
      # We are doing the ungrouping only if the last age interval is 95
      # AND 
      # the death count in the group is higher than 2 
      # (a small enough number to be a meaningful process). 
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

# Since we have the ungroup function ready 
# we can prepare the data
key <- c("region", "period", "sex", "cause_name")

threshold = 75 # threshold age

gbd2 <- gbd %>% 
  filter(x >= threshold) %>%  # use only data above threshold for ungrouping
  unite("key", all_of(key), sep = "-", remove = FALSE) 

cases <- gbd2 %>% 
  filter(
    cause_name != "Neonatal Disorders",
    cause_name != "Maternal disorders",
  ) %>% 
  select(key) %>% 
  unique() %>% 
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
  arrange(cause_name, region, period, sex, x)

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
  complete(x, nesting(region, sex, period, cause_name)) %>% 
  mutate(
    deaths = replace_na(deaths, 0),
  )

# ------------------------------------------
# CHECK POINT: the death counts should remain about the same after the 
# redistribution in the last age intervals.
# 
# gbd %>% 
#   filter(
#     period == 2021,
#     sex == "male",
#   ) %>% 
#   select(deaths) %>% sum 
# 
# 
# GBD %>%
#   filter(
#     period == 2021,
#     sex == "male",
#   ) %>% 
#   select(deaths) %>% sum 

# ------------------------------------------
# include data in the package
rank <- read_excel(
  path = path_file_map,
  sheet = "Cause Hierarchy") %>%
  clean_names() %>%
  filter(
    sdg_selection != "no") %>%
  arrange(sdg_order) %>% 
  select(sdg_selection) %>% 
  unlist() %>% 
  unique()


GBD[!complete.cases(GBD),]

data_gbd2021_sdg <- GBD %>%
  mutate(
    cause_name = factor(cause_name, levels = rank),
    region = factor(region)
  ) %>% 
  drop_na()


dt <- format(Sys.Date(), '%Y%m%d')
save(data_gbd2021_sdg, file = paste0("data-raw/IHME_GBD2021_Data/data_gbd2021_sdg_", dt,".Rdata"))
usethis::use_data(data_gbd2021_sdg, overwrite = TRUE)




