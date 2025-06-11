# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Fri Jun  6 08:29:06 2025
# ------------------------------------------------- #


# HERE we import the source code of several functions in order to reduce the 
# dependency of our package on other packages and avoid future trouble caused 
# by updates in the related libraries. We do this only when we are using
# 1 or 2 simple functions from a third party library.


#' scales::label_number_si - function 
#' @keywords internal 
label_number_si <- function (accuracy = 1, unit = NULL, sep = NULL, ...) {
  sep <- if (is.null(unit)) "" else " "
  force_all <- function(...) list(...)
  force_all(accuracy, ...) 
  function(x) {
    breaks <- c(0, 10^c(K = 3, M = 6, B = 9, T = 12))
    n_suffix <- cut(abs(x), breaks = c(unname(breaks), Inf), 
                    labels = c(names(breaks)), right = FALSE)
    n_suffix[is.na(n_suffix)] <- ""
    suffix <- paste0(sep, n_suffix, unit)
    scale <- 1/breaks[n_suffix]
    scale[which(scale %in% c(Inf, NA))] <- 1
    number(x, accuracy = accuracy, scale = unname(scale), 
           suffix = suffix, ...)
  }
}

#' scales::number - function 
#' @keywords internal 
number <- function (x, accuracy = NULL, scale = 1, prefix = "", suffix = "", 
          big.mark = " ", decimal.mark = ".", trim = TRUE, ...) 
{
  if (length(x) == 0) 
    return(character())
  accuracy <- accuracy #%||% precision(x * scale)
  round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}
  x <- round_any(x, accuracy/scale)
  nsmall <- -floor(log10(accuracy))
  nsmall <- min(max(nsmall, 0), 20)
  ret <- format(scale * x, big.mark = big.mark, decimal.mark = decimal.mark, 
                trim = trim, nsmall = nsmall, scientific = FALSE, ...)
  ret <- paste0(prefix, ret, suffix)
  ret[is.infinite(x)] <- as.character(x[is.infinite(x)])
  ret[is.na(x)] <- NA
  names(ret) <- names(x)
  ret
}


# Hack CRAN check warnings related to tidyverse coding style
globalVariables(
  c(
    ".",
    "cause_name",
    "cause_name", 
    "deaths", 
    "decomposition", 
    "llx",
    "period", 
    "region", 
    "sex",
    "ttx",
    "x", 
    "x.int",
    "x_int"
  )
)
