# {lemur} - Life expectancy monitor upscaled in R

### R package and Shiny application

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![version](https://img.shields.io/badge/version-1.8.0-blue.svg)](https://github.com/mpascariu/lemur/blob/main/DESCRIPTION)
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

Everything runs on the bundled Global Burden of Disease 2021 (GBD2021)
estimates from IHME: 220 regions from 1990 to 2021, three sex categories
(male, female and the two sexes combined) and 18 broad cause groups, arranged
in abridged life tables of 25 age groups. 

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

The package bundles the GBD2021 datasets (exposed as `data_gbd2021_lt()`,
`data_gbd2021_cod()` and `data_gbd2021_sdg()`), so you can run the examples
on the help pages and the analysis functions without a database connection.

## Documentation

The full documentation -- a complete worked analysis of the GBD2021 data
with interactive figures -- is published as a web page:

**<https://mpascariu.github.io/lemur/>**

The same document ships with the package and opens straight from R:
`vignette("lemur-intro")`.

[![App
Screenshot](inst/app/www/app_lemur_20260812.png)](https://mpascariu.github.io/lemur/)

## The Shiny application

Everything is also wrapped in an interactive dashboard, launched with a
single call:

``` r
lemur::run_app()          # local data mode (bundled GBD2021 data)
```

Five analysis modes -- scenario analysis within a region, region
comparisons, sex comparisons, and two SDG target modes -- plus a data tab,
a methods tab and an interactive map, all rendered natively in plotly.

## Next steps

* Every exported function has a worked example: `?modify_life_table`,
  `?decompose_by_cod`, `?plot_decompose`, `?data_gbd2021_lt`.
* The decomposition methods follow Andreev, Shkolnikov and Begun (2002),
  *Algorithm for decomposition of differences between aggregate demographic
  measures*, Demographic Research 7, 499-522.
* The data are the Global Burden of Disease Study 2021 results (GBD 2021),
  Institute for Health Metrics and Evaluation (IHME), 2022
  (<https://vizhub.healthdata.org/gbd-results/>). The package is being
  updated to GBD 2023 data.
* Source code, issues and releases: <https://github.com/mpascariu/lemur>.
