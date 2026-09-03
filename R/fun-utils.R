# ------------------------------------------------- #
# Author: Marius D. Pascariu
# ------------------------------------------------- #

# HERE we import the source code of several functions in order to reduce the 
# dependency of our package on other packages and avoid future trouble caused 
# by updates in the related libraries. We do this only when we are using
# 1 or 2 simple functions from a third party library.

#' scales::label_number_si (vendored)
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

#' scales::number (vendored)
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


#' shinyWidgets::setSliderColor (vendored)
#'
#' Edit the color of Shiny's slider inputs, returning the CSS that recolors the
#' slider skin. Used internally to colour the app's sliders; not part of the
#' public API.
#' @keywords internal
setSliderColor_ <- function(color, sliderId) {
  stopifnot(!is.null(color))
  stopifnot(is.character(color))
  stopifnot(is.numeric(sliderId))
  stopifnot(!is.null(sliderId))
  stopifnot(length(color) >= length(sliderId))

  sliderId <- sliderId - 1

  sliderCol <- lapply(seq_along(sliderId), function(j) {
    i <- sliderId[j]
    paste0(
      ".js-irs-", i, " .irs-single,",
      " .js-irs-", i, " .irs-from,",
      " .js-irs-", i, " .irs-to,",
      " .js-irs-", i, " .irs-bar-edge,",
      " .js-irs-", i,
      " .irs-bar{  border-color: transparent;background: ", color[j],
      "; border-top: 1px solid ", color[j],
      "; border-bottom: 1px solid ", color[j],
      ";}"
    )
  })

  tags$head(tags$style(HTML(paste(sliderCol, collapse = "\n"))))
}

# Hack CRAN check warnings related to tidyverse coding style
globalVariables(
  c(
    ".",
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

# Move a column into the row names of a data frame. Base replacement for
# tibble::column_to_rownames() (see fun-modify_LT.R / build_cod_matrix).
#' @keywords internal
df_rownames_from <- function(df, col) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  row.names(df) <- df[[col]]
  df[[col]] <- NULL
  df
}

# Move the row names of a data frame into a new leading column. Base
# replacement for tibble::rownames_to_column().
#' @keywords internal
df_add_rownames <- function(df, col) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df[[col]] <- row.names(df)
  row.names(df) <- NULL
  df
}

# ----------------------------------------------------------------------------
# FUNCTIONS used in the Server

#' @keywords internal
format_datatable <- function(data, caption){
  DT::datatable(
    data = format(
      x = as.data.frame(data),
      big.mark = ",",
      scientific = FALSE,
      digits = 3
    ),
    caption  = caption,
    rownames = FALSE,
    # filter  = 'top',
    options = list(
      # dom = 't',
      pageLength = 25,
      autoWidth = TRUE
    )
  )
}

#' @keywords internal 
reset_inputs <- function(session) {
  # Define all reset values in one place
  reset_values <- list(
    sex = list(inputId = "sex", value = "both", type = "radio"),
    perc = list(inputId = "perc", value = FALSE, type = "switch"),
    fig2_x = list(inputId = "fig2_x", value = seq(0, 95, 10), type = "select"),
    time_slider = list(inputId = "time_slider", value = 2023, type = "sliderText"),
    age_change = list(inputId = "age_change", value = c(0, 95), type = "sliderText"),
    cod_change = list(inputId = "cod_change", value = 0, type = "slider"),
    cod_target = list(inputId = "cod_target", value = lemur::data_app_input$cause_name, type = "checkbox")
  )
  
  # SDG sliders (1-7 and 2_1-2_21)
  sdg_sliders <- c(paste0("sdg_", 1:7), paste0("sdg2_", 1:21))
  for (slider_id in sdg_sliders) {
    reset_values[[slider_id]] <- list(inputId = slider_id, value = 0, type = "slider")
  }
  
  # Apply all resets with a loop
  for (input_config in reset_values) {
    switch(
      input_config$type,
      radio = shinyjs::runjs(sprintf(
        "Shiny.setInputValue('%s', '%s');", 
        input_config$inputId, input_config$value)),
      switch     = updateSwitchInput(session, input_config$inputId, value = input_config$value),
      select     = updateSelectInput(session, input_config$inputId, selected = input_config$value),
      sliderText = updateSliderTextInput(session, input_config$inputId, selected = input_config$value),
      slider     = updateSliderInput(session, input_config$inputId, value = input_config$value),
      checkbox   = updatePrettyCheckboxGroup(session, input_config$inputId, selected = input_config$value)
    )
  }
}
