
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {lemur} - Life expectancy monitor upscaled in R

### R package and Shiny application

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![issues](https://img.shields.io/github/issues-raw/mpascariu/lemur.svg)](https://github.com/mpascariu/lemur/issues)
[![license](https://img.shields.io/badge/License-GNU%20GPLv3-blue.svg)](https://github.com/mpascariu/lemur/blob/master/LICENSE)

<!-- badges: end -->

The life expectancy monitoring tool allows the user to selected
mortality changes over the entire lifespan or at specific ages, as well
as for overall mortality or for specific causes of death. For example,
how would life expectancy look if cardiovascular mortality were to be
reduced by 50%? Or how would life expectancy look if infant mortality
was eliminated? The tool facilitates assessing changes and comparisons
in life expectancy under those selected scenarios of mortality change.
Furthermore, the tool lets the user compare cause-of-death profiles and
life expectancies across time, countries and sexes.

The tool is hosted by the **HASS Digital Research Hub (HDRH)** at the
Australian National University (ANU).

## Installation

Once you have an R session open (in RStudio or the R
console), install the package straight from
[GitHub](https://github.com/mpascariu/lemur):

``` r
# 1. Install the devtools package, if you do not have it yet
install.packages("devtools")

# 2. Install lemur from GitHub (latest version on the main branch)
devtools::install_github("mpascariu/lemur")
```

The package bundles the GBD2021 datasets (exposed as `data_gbd2021_lt()`,
`data_gbd2021_cod()` and `data_gbd2021_sdg()`), so you can run the examples
on the help pages and the analysis functions without a database connection.

## Example

This is a basic example which shows you how to launch the monitor in
your browser:

``` r
lemur::run_app()
```

[![App
Screenshot](inst/app/www/app_lemur_20250928.png)](https://github.com/mpascariu/lemur)

All the simulations done in the monitor can be executed using the `R`
syntax directly in the R/Rstudio console. The package data is available
via the accessor functions `data_gbd2021_cod()` and `data_gbd2021_lt()`
or the help pages of relevant functions like `decompose_by_cod()`,
`modify_life_table()` and the related `plot_` functions.
