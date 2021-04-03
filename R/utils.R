# --------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Sat Apr 03 15:24:36 2021
# --------------------------------------------------- #


#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @usage lhs \%>\% rhs
#' @export
NULL


# ----------------------------------------------------------------------------
# Hack CRAN check warnings related to tidyverse coding style
globalVariables(
  c(
    "region", 
    "period", 
    "sex",
    "x",
    "level",
    "deaths",
    "cause_name",
    "perc"
    )
)