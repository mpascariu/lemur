# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed Apr 27 2021
# --------------------------------------------------- #

# HERE we import the source code of several functions in order to reduce the 
# dependency of our package on other packages and avoid future trouble caused 
# by updates in the related libraries. We do this only when we are using
# 1 or 2 simple functions from a third party library.

#' shinydashboard::box - function 
#' @keywords internal 
box <- function (..., title = NULL, footer = NULL, status = NULL, 
                 solidHeader = FALSE, background = NULL, width = 6, 
                 height = NULL, collapsible = FALSE, collapsed = FALSE) {
  
  boxClass <- "box"
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if (!is.null(status)) {
    # validateStatus(status)
    boxClass <- paste0(boxClass, " box-", status)
  }
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
  }
  if (!is.null(background)) {
    # validateColor(background)
    boxClass <- paste0(boxClass, " bg-", background)
  }
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", validateCssUnit(height))
  }
  titleTag <- NULL
  if (!is.null(title)) {
    titleTag <- h3(class = "box-title", title)
  }
  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- status #%OR% "default"
    collapseIcon <- if (collapsed) 
      "plus"
    else "minus"
    collapseTag <- div(class = "box-tools pull-right", 
                       tags$button(class = paste0("btn btn-box-tool"), 
                                   `data-widget` = "collapse", 
                                   shiny::icon(collapseIcon)))
  }
  headerTag <- NULL
  if (!is.null(titleTag) || !is.null(collapseTag)) {
    headerTag <- div(class = "box-header", titleTag, collapseTag)
  }
  div(class = if (!is.null(width)) 
    paste0("col-sm-", width), div(class = boxClass, style = if (!is.null(style)) 
      style, headerTag, div(class = "box-body", ...), if (!is.null(footer)) 
        div(class = "box-footer", footer)))
}


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
