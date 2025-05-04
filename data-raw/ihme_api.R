# -------------------------------------------------------------- #
# Title: SDG API only
# Author: Marius D. PASCARIU
# Last Update: Sun Oct  6 14:14:25 2024
# -------------------------------------------------------------- #
remove(list = ls())
library(tidyverse)
library(httr)
library(jsonlite)

## https://api.healthdata.org/sdg/v1/docs#/

url      <- "https://api.healthdata.org/sdg/v1/GetResultsByLocation?location_id=27&year=2004"
headers  <- c('Content-Type' = 'application/json', 'Authorization' = 'oni8h2ak2zzmxrx2o8naidt96ancyaen')
response <- GET(url, add_headers(.headers = headers))
dt       <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
dt       <- as_tibble(dt$results)

dt$indicator_name %>% unique()















