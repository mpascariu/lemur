# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Thu Nov 11 17:01:07 2021
# --------------------------------------------------- #

#' Modify life table by changing the cause of death associated risks
#'
#' @param lt Life table;
#' @param cod Causes of death matrix containing death counts corresponding
#' to the population and time period of the life table;
#' @param cod_change Numerical scalar, vector or matrix.
#' The changes to be applied to the causes of death rate, m[c,x],
#' in order to reduce or increase the mortality estimates given by the life
#' table. Accepted input: any value greater than -100. See examples.
#' @return A life table in the same format as the input life table.
#' @export
modify_life_table <- function(lt, cod, cod_change) {
  # death counts by cod from a long dataset
  lv  <- as.character(unique(cod$cause_name))
  cod <- build_cod_matrix(cod)[, lv]
  qx  <- replace_na(lt$qx, 0)

  # Modify cod matrix by applying a change
  mod_cod <- modify_cod(cod, cod_change)

  # reduced probability of survival by cod
  pxi_r <- (1 - qx) ^ mod_cod

  # all-cause reduced qx
  qx_r <- 1 - apply(pxi_r, 1, prod)

  # Build life-table from qx_r using standard procedure
  # from {MortalityLaws}. The sex argument is not required since we have the
  # a[x] which would give us a more accurate construction.
  LT <- LifeTable(
    x   = lt$x,
    qx  = qx_r,
    ax  = lt$ax
  )

  # Exit
  # The output should have the same format as the input life table
  out <- lt %>%
    select(
      -all_of(names(LT$lt))
    ) %>%
    bind_cols(LT$lt)

  return(out)
}


#' Modify COD table by changing the cause of death associated risks
#'
#' @inheritParams modify_life_table
#' @return A long table with the same format as the input data
#' @export
modify_cod_table <- function(cod, cod_change){

  x = cause_name = deaths = . <- NULL

  #build cod matrix
  cod2 <- cod %>%
    select(x, cause_name, deaths) %>%
    # and build a matrix
    pivot_wider(
      names_from = cause_name,
      values_from = deaths
    ) %>%
    arrange(x)  %>%
    # replace na with 0
    mutate_all(~replace(., is.na(.), 0)) %>%
    # name rows
    column_to_rownames("x") %>%
    as.matrix()

  lv <- as.character(unique(cod$cause_name))
  cod2 <- cod2[, lv]

  # Modify cod matrix by applying a change
  mod_cod <- modify_cod(cod2, cod_change)

  # Go from matrix to long table
  out <- mod_cod %>%
    as.data.frame() %>%
    rownames_to_column(., var = "x") %>%
    pivot_longer(
      cols = -x,
      names_to = "cause_name",
      values_to = "deaths") %>%
    mutate(
      x = as.numeric(x),
      cause_name = factor(as.character(cause_name), levels = lv)) %>%
    arrange(cause_name)
    # remove original deaths column and join the datasets
  out <- left_join(cod[-6], out, by = c("x", "cause_name"))

  # Exit
  return(out)
}


#' Format COD data
#'
#' Transform the COD count data from a long table to a matrix
#' containing percentages, with ages as rows and cod's as columns.
#'
#' @param cod COD long table
#' @return A matrix with percentages.
#' @export
build_cod_matrix <- function(cod) {

  region = period = sex = x = deaths = cause_name = perc <- NULL

  M <- cod %>%
    # compute percentages of each disease for
    # given age-region-period-sex and across ages
    group_by(region, period, sex, x) %>%
    mutate(perc = deaths / sum(deaths)) %>%
    ungroup() %>%
    select(x, cause_name, perc) %>%
    # and build a matrix
    pivot_wider(
      names_from = cause_name,
      values_from = perc
    ) %>%
    arrange(x)  %>%
    # replace na with 0
    mutate_all(~replace(., is.na(.), 0)) %>%
    # name rows
    column_to_rownames("x") %>%
    as.matrix()

  L <- rowSums(M) < 0.999
  if (any(L)) {
    M[L, ] <- 1/ncol(M)
  }

  return(M)
}


#' Modify COD values by changing the cause of death associated risks
#'
#' @inheritParams modify_life_table
#' @return A long table with the same format as the input cod
#' @keywords internal
modify_cod <- function(cod, cod_change) {

  # Reduction
  r   <- 1 + cod_change / 100

  # reduced probability of survival by cod
  if (all(r <= 0)) {
    stop(
      paste(
        "The mortality reduction cannot be 100% or more.",
        "That would make us immortals; and this software",
        "does not know how to deal with that!",
        call. = FALSE)
    )

  } else if (is.matrix(cod_change)) {
    out <- cod * r

  } else {
    # is cod_change is a vector then we have to transpose to
    # do the multiplication correctly
    out <- t(t(cod) * r)

  }

  return(out)
}


#' Build reduction matrix to be used in the app
#' @keywords internal
#' @export
build_reduction_matrix <- function(
  data,
  select_cod,
  select_x,
  cod_change) {

  cn <- as.character(unique(data$cause_name))
  rn <- unique(data$x)

  mat <- matrix(0,
                ncol = length(cn),
                nrow = length(rn),
                dimnames = list(rn, cn))

  select_ages <- rn %in% min(select_x):max(select_x)
  mat[select_ages, select_cod] <- cod_change

  return(as.matrix(mat))
}






