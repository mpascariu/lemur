# {lemur} - Life expectancy monitor upscaled in R

### R package and Shiny application

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![version](https://img.shields.io/badge/version-2.0.4-blue.svg)](https://github.com/mpascariu/lemur/blob/main/DESCRIPTION)
[![issues](https://img.shields.io/github/issues-raw/mpascariu/lemur.svg)](https://github.com/mpascariu/lemur/issues)
[![license](https://img.shields.io/badge/License-GNU%20GPLv3-blue.svg)](https://github.com/mpascariu/lemur/blob/master/LICENSE)

<!-- badges: end -->

## What is lemur?

`lemur` ("Life expectancy monitor upscaled in R") is an R package and
Shiny application for scenario analysis of mortality. Given the cause-of-death
distribution of a population, you ask *"what if?"* questions -- *what would
life expectancy be if cardiovascular mortality fell by 50%?* -- and the
package recomputes the life table, shows you the gains and losses at every
age, and decomposes the change by age and cause of death. It also compares
cause-of-death profiles and life tables between regions, sexes and time
periods.

Everything runs on the bundled Global Burden of Disease 2023 (GBD 2023)
estimates from IHME: 216 regions and the calendar-year-2023 data included -- a
1990 to 2023 span, three sex categories (male, female and the two sexes
combined) and 18 broad cause groups, arranged in abridged life tables of 22
age groups (the terminal 95+ interval open). 

The tool is hosted by the **HASS Digital Research Hub (HDRH)** at the
Australian National University (ANU).

## Installation

Once you have an R session open (in RStudio or the R console), install the
package straight from [GitHub](https://github.com/mpascariu/lemur):

lemur requires **R version 4.3.0 or newer**.

``` r
# 1. Install the pak package, if you do not have it yet
install.packages("pak")

# 2. Install lemur from GitHub (latest version on the main branch)
pak::pak("mpascariu/lemur")
```

The package bundles the GBD 2023 datasets (exposed as `data_gbd_lt()`,
`data_gbd_cod()` and `data_gbd_sdg()`), covering deaths and life tables from
1990 to 2023, so you can run the examples on the help pages and the analysis
functions without a database connection.

## Run with Docker (no R required)

The app ships as a prebuilt image on the GitHub Container Registry:

``` bash
docker pull ghcr.io/mpascariu/lemur-shiny:latest

# quick start -- bundled data, local mode:
docker run -d --name lemur -p 3838:3838 ghcr.io/mpascariu/lemur-shiny:latest \
  R -e "options(shiny.port = 3838, shiny.host = '0.0.0.0'); lemur::run_app(lb = FALSE)"
# then open http://localhost:3838
```

For the full server deployment (PostgreSQL-backed, REST API included) see
[docs/docker_running_guide.md](docs/docker_running_guide.md); building the
images yourself is covered in
[docs/docker_building_guide.md](docs/docker_building_guide.md).

## Documentation

The full documentation -- a complete worked analysis of the GBD data
with interactive figures -- is published as a web page:

**<https://mpascariu.github.io/lemur/>**

The same document ships with the package and opens straight from R:
`vignette("lemur-intro")`.

## The Shiny application

[![App
Screenshot](inst/app/www/app_lemur_20260812.png)](https://mpascariu.github.io/lemur/)

Everything is also wrapped in an interactive dashboard, launched with a
single call:

``` r
lemur::run_app()          # local data mode (bundled GBD data)
```

Five analysis modes -- scenario analysis within a region, region
comparisons, sex comparisons, and two SDG target modes -- plus a data tab,
a methods tab and an interactive map, all rendered natively in plotly.

## References

* Every exported function has a worked example: `?modify_life_table`,
  `?decompose_by_cod`, `?plot_decompose`, `?data_gbd_lt`.
* The decomposition methods follow Andreev, Shkolnikov and Begun (2002),
  *Algorithm for decomposition of differences between aggregate demographic
  measures*, Demographic Research 7, 499-522.
* The data are the Global Burden of Disease Study 2023 (GBD 2023) results,
  Institute for Health Metrics and Evaluation (IHME)
  (<https://vizhub.healthdata.org/gbd-results/>). The calendar-year-2023 data
  is included; deaths and life tables span 1990-2023.
* Source code, issues and releases: <https://github.com/mpascariu/lemur>.
