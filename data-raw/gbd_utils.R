# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: 2026-08-31
# ------------------------------------------------- #
#
# Shared helpers for the combined GBD2021 + GBD2023 processing pipeline.
#
# The package's cause-of-death and life-table data used to be built from a
# single GBD 2021 download. These helpers let the three process scripts below
# (process_gbd_cod.R / process_gbd_sdg.R / process_gbd_lt.R) read the 2021 and
# 2023 downloads side by side, apply one consistent location & cause mapping
# to both rounds, and emit a single combined dataset. The original scripts
# ungrouped the terminal 95+ interval with pclm and extended the life tables
# with a Kannisto law; this pipeline drops both and keeps 95+ as an open
# interval throughout.
#
# Source data (gitignored, under data-raw/):
#   IHME_GBD2021_Data/  CoD_Level_2/_Level_3 zips (deaths, 1990-2021)
#   IHME_GBD2023_Data/  IHME-GBD_2023-*.zip   (deaths, 2023)
#                       IHME-GBD_LT_*.zip     (life-table qxn, 1990-2023)
#
# Consistency decisions agreed with the maintainer:
#   * "Africa/Americas/Asia/Europe" macro regions exist in 2021 but not in the
#     2023 download -> drop them from BOTH rounds so the combined data matches.
#   * The 2021 hierarchy mislabels GBD location 7 (Democratic People's Republic
#     of Korea / North Korea) as "South Korea". Fix symmetrically:
#       id 7  -> "North Korea"   id 68 (Republic of Korea) -> "South Korea"
#   * Region names standardize on the 2021 names; 2023 locations are mapped
#     onto them by location_id.
#   * COD / SDG combine the 2021-round deaths (1990-2021) with the 2023-round
#     deaths (2023). Life tables use only the 2023-round qxn (1990-2023).

# Locations dropped from both rounds (the four custom GBD macro regions that
# the 2021 download included but the 2023 one did not): Africa, Americas,
# Asia, Europe.
MACRO_DROP_IDS <- c(44559L, 44560L, 44561L, 44562L)

# Korea relabel keyed on location_id (see header).
KOREA_FIX <- data.frame(
  location_id = c(7L, 68L),
  region      = c("North Korea", "South Korea"),
  stringsAsFactors = FALSE
)

# Location / cause hierarchy (the 2021 tools guide xlsx). Cause and location
# ids are stable across GBD rounds, so this one file keys both downloads.
HIERARCHY <- file.path(
  "data-raw", "GBD_2021_Data_Tools_Guide",
  "IHME_GBD_2021_A1_HIERARCHIES_Y2024M05D15.XLSX")

# The all-cause GBD cause id, used as the reference total in consistency
# checks (sum of cause-specific deaths should equal it).
ALL_CAUSE_ID <- 294L


# ---- Age band -> numeric x ----
# Identical mapping to the original scripts: "<1 year" -> 0, "12-23 months" -> 1,
# "95+ years" -> 95, other bands take their lower limit ("2-4 years" -> 2,
# "5-9 years" -> 5, ...). "All ages" comes back as NA and is dropped upstream.
age_to_x <- function(age_name) {
  x <- ifelse(age_name == "<1 year", 0, age_name)
  x <- ifelse(x == "12-23 months", 1, x)
  x <- ifelse(x == "95+ years", 95, x)
  x <- sub("-.*", "", x)
  as.numeric(x)
}


# ---- Read the hierarchy sheets ----
#' @return list(loc, cod) with the cleaned location and cause hierarchies.
read_hierarchy <- function() {
  loc <- readxl::read_excel(HIERARCHY, sheet = "GBD 2021 Locations Hierarchy") %>%
    janitor::clean_names()
  cod <- readxl::read_excel(HIERARCHY, sheet = "Cause Hierarchy") %>%
    janitor::clean_names()
  list(loc = loc, cod = cod)
}


# ---- Canonical region map: location_id -> region name ----
# Standardizes on the 2021 names: countries (level 3) keep their title-case
# name, everything coarser is uppercased (as the original scripts did), then
# the Korea relabel is applied. Macro-drop ids are still present here; they are
# filtered at the data-shaping stage.
build_location_map <- function(loc_hierarchy) {
  loc_hierarchy %>%
    dplyr::select(location_id, location_name, level) %>%
    dplyr::mutate(region = ifelse(level == 3, location_name, toupper(location_name))) %>%
    dplyr::select(-location_name, -level) %>%
    dplyr::left_join(KOREA_FIX, by = "location_id") %>%
    # the join collides on `region`, so the map becomes region.x / region.y;
    # coalesce prefers the forced Korea label, else the locale-3 name.
    dplyr::mutate(region = dplyr::coalesce(region.y, region.x)) %>%
    dplyr::select(location_id, region)
}


# ---- Unzip one or more zips and read the CSVs inside ----
# data.table::fread is several times faster than readr on these multi-million
# row tables; the result is coerced to a tibble so the dplyr pipeline below is
# unchanged. `fill`/`use.names` let the Level_3 zips (fewer columns) bind to the
# Level_2 zips.
read_zips_csv <- function(zips) {
  td <- tempfile(); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  chunks <- vector("list", length(zips))
  for (i in seq_along(zips)) {
    utils::unzip(zips[i], exdir = td)
    files <- list.files(td, pattern = "[.]csv$", full.names = TRUE)
    if (length(files)) {
      chunks[[i]] <- data.table::rbindlist(
        lapply(files, data.table::fread), fill = TRUE, use.names = TRUE)
    } else {
      chunks[[i]] <- NULL
    }
  }
  dplyr::as_tibble(data.table::rbindlist(chunks, fill = TRUE, use.names = TRUE))
}


# ---- Read GBD 2021 deaths (CoD Level_2 + selected Level_3) ----
# The full Level_2 set carries all the death counts; the handful of Level_3
# subgroups we report separately must be read too (their deaths sit inside the
# Level_2 parents and are subtracted back out later). `level3_causes` lists the
# Level_3 cause names to keep, which prunes the large Level_3 files early.
#
# The 2021 Level_2 download is delivered as several overlapping chunk zips, so
# the same (location, cause, sex, age, year) cell appears multiple times with
# identical values. Deduplicate on that key -- the original script happened to
# sum duplicates away in its aggregation, but the reduction step here would
# otherwise double-count them.
dedup_deaths <- function(raw) {
  dplyr::distinct(raw,
                  location_id, cause_id, sex_name, age_name, year,
                  .keep_all = TRUE)
}
read_deaths_2021 <- function(level3_causes) {
  base <- "data-raw/IHME_GBD2021_Data"
  lev2 <- read_zips_csv(
    list.files(file.path(base, "CoD_Level_2"), pattern = "[.]zip$", full.names = TRUE))
  lev3 <- read_zips_csv(
    list.files(file.path(base, "CoD_Level_3"), pattern = "[.]zip$", full.names = TRUE))
  lev3 <- lev3[lev3$cause_name %in% level3_causes, ]
  dedup_deaths(dplyr::bind_rows(lev2, lev3))
}


# ---- Read GBD 2023 deaths (flat CSVs) ----
# Names start "IHME-GBD_2023-" so the life-table zips ("IHME-GBD_LT_") are not
# matched.
read_deaths_2023 <- function() {
  base <- "data-raw/IHME_GBD2023_Data"
  zips <- list.files(base, pattern = "^IHME-GBD_2023-.*[.]zip$", full.names = TRUE)
  dedup_deaths(read_zips_csv(zips))
}


# ---- Read GBD 2023 life-table (probability of death) data ----
read_lt_2023 <- function() {
  base <- "data-raw/IHME_GBD2023_Data"
  zips <- list.files(base, pattern = "^IHME-GBD_LT_.*[.]zip$", full.names = TRUE)
  read_zips_csv(zips) %>%
    dplyr::distinct(location_id, sex_name, age_name, year, .keep_all = TRUE)
}


# ---- Shape raw GBD deaths into the long analysis schema ----
# Input: a raw deaths frame with columns location_id, sex_name, age_name,
# year, cause_id, val. Output: region, sex, period, x, cause_id, deaths
# (rows that do not map to a kept location, or that are not a numeric age,
# are dropped).
shape_deaths <- function(raw, location_map) {
  raw %>%
    dplyr::filter(!is.na(cause_id)) %>%
    dplyr::left_join(location_map, by = "location_id") %>%
    dplyr::filter(!is.na(region)) %>%
    dplyr::filter(!location_id %in% MACRO_DROP_IDS) %>%
    dplyr::mutate(
      x      = age_to_x(age_name),
      sex    = tolower(sex_name),
      period = year
    ) %>%
    dplyr::filter(!is.na(x)) %>%
    dplyr::select(region, sex, period, x, cause_id, deaths = val)
}


# ---- All-cause reference totals (for consistency checks) ----
# The all-cause row (cause id 294) is present in both downloads and is what the
# sum of the cause-specific categories should reproduce after the
# Level_3-from-Level_2 subtractions.
allcause_reference <- function(raw, location_map) {
  raw %>%
    dplyr::filter(!is.na(cause_id), cause_id == ALL_CAUSE_ID) %>%
    dplyr::left_join(location_map, by = "location_id") %>%
    dplyr::filter(!is.na(region), !location_id %in% MACRO_DROP_IDS) %>%
    dplyr::mutate(
      x      = age_to_x(age_name),
      sex    = tolower(sex_name),
      period = year
    ) %>%
    dplyr::filter(!is.na(x)) %>%
    dplyr::group_by(region, sex, period, x) %>%
    dplyr::summarise(deaths_ref = sum(val), .groups = "drop")
}


# ---- Add the "both" sex (male + female summed) ----
add_both_sex <- function(d) {
  both <- d %>%
    dplyr::group_by(region, period, x, cause_name) %>%
    dplyr::summarise(deaths = sum(deaths), .groups = "drop") %>%
    dplyr::mutate(sex = "both", .after = region)
  dplyr::bind_rows(d, both)
}


# ---- Complete the grid and turn causes into a factor ----
# Every region x sex x period x x x cause combination is materialised (missing
# ones become zero), which the app's matrix algebra expects.
finalise <- function(d, cause_levels) {
  d %>%
    dplyr::mutate(cause_name = factor(cause_name, levels = cause_levels)) %>%
    tidyr::complete(x, tidyr::nesting(region, sex, period, cause_name)) %>%
    dplyr::mutate(deaths = tidyr::replace_na(deaths, 0)) %>%
    dplyr::filter(!is.na(region), !is.na(sex), !is.na(period), !is.na(cause_name))
}
