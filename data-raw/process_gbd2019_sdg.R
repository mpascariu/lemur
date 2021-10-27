# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed Oct 27 21:32:53 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)
library(stringr)
library(readr)
library(readxl)
library(janitor)
library(ungroup)

# This script is similar with the "process_gbd2019_cod_v2.R".
# Here we are creating a similar dataset but following a different
# groupping based on the US's SDG goals. This means we have to include 
# more level 3 COD's.
# See IHME_GBD_2019_A1_HIERARCHIES_Y2020M10D15.XLSX for a mapping of the 
# new groups.
# 
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

# replace 'cod_selection' with 'sdg_selection' everywhere in the script
map <- read_excel(
  path = path_file_map,
  sheet = "Cause Hierarchy") %>%
  clean_names()

cod_map <- read_excel(
  path = path_file_map,
  sheet = "Cause Hierarchy") %>%
  clean_names() %>%
  filter(sdg_selection != "no") %>% 
  select(
    cause_id, 
    sdg_selection,
    cod_selection)


# ------------------------------------------.
# GBD COD data wrangling

gbd <- left_join(gbd_cod, cod_map, by = "cause_id") %>% 
  filter(
    sdg_selection != "no",
    age_name != "All Ages",
  ) %>% 
  select(
    # drop few columns
    location_name,
    sex_name,
    age_name,
    year,
    sdg_selection,
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
    cause_name = sdg_selection,
    region = location_name,
    sex = sex_name,
    period = year,
    median = val
  ) %>%
  pivot_longer(cols = median:lower, names_to = "level", values_to = "deaths") %>% 
  filter(level == "median") %>% 
  # group-by everything except 'val' and 'year' columns
  # so that we can aggregate data across years
  group_by_at(setdiff(names(.), c("deaths"))) %>% 
  summarise(deaths = sum(deaths)) %>%
  ungroup() %>% 
  complete(x, nesting(region, sex, period, cause_name, level)) %>% 
  mutate(deaths = replace_na(deaths, 0)) %>% 
  group_by(region, sex, period, x, level) %>% 
  mutate(
    deaths = ifelse(cause_name == 'Respiratory Infections (excl. Tuberculosis)', deaths - deaths[cause_name == 'Tuberculosis'], deaths), 
    deaths = ifelse(cause_name == 'Neglected Tropical Diseases (excl. Malaria)', deaths - deaths[cause_name == 'Malaria'], deaths),
    deaths = ifelse(cause_name == 'Kidney Disease', deaths - deaths[cause_name == 'Diabetes'], deaths),
    deaths = ifelse(cause_name == 'Injuries (excl. Poisonings)', deaths - deaths[cause_name == 'Poisonings'] - deaths[cause_name == 'Exposure to Forces of Nature'], deaths),
    deaths = ifelse(cause_name == 'Interpersonal Violence', deaths - deaths[cause_name == 'Self-Harm'], deaths),
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
  
  if (n != 0 & sum(X$deaths) > 10) {
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
    cause_name != "Maternal disorders",
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
GBD <- bind_rows(gbd110, gbd_both) 

# ------------------------------------------
# CHECK POINT: 


sum(gbd_level2$val)
GBD %>% 
  filter(level == "median") %>% 
  select(deaths) %>% 
  sum()/2
# We can notice a difference between Level2 data and processed data
# due to ungrouping residuals.

# ------------------------------------------
# include data in the package
rank <- GBD %>%
  filter(sex == "both",
         level == "median") %>% 
  group_by(cause_name) %>% 
  summarise(value = sum(deaths)) %>% 
  arrange(value)


data_gbd2019_sdg <- GBD %>%
  filter(level == "median") %>% 
  mutate(
    cause_name = factor(cause_name, levels = rank$cause_name),
    region = factor(region)
  )

# save(data_gbd2019_sdg, file = "data-raw/data_gbd2019_sdg.Rdata")
# load("data-raw/data_gbd2019_sdg.Rdata")

usethis::use_data(data_gbd2019_sdg, overwrite = TRUE)

# ----------------------------------------------------------------------------



D <- data_gbd2019_sdg

data_app_input <- MortalityCauses::data_app_input

data_app_input$cause_name_sdg <- levels(D$cause_name)


usethis::use_data(data_app_input, overwrite = TRUE)





