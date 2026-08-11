# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Sun Apr  6 20:31:46 2025
# ------------------------------------------------- #

#' Modify life table by changing the cause of death associated risks
#'
#' @param lt Life table.
#' @param cod Causes of death matrix containing death counts corresponding
#' to the population and time period of the life table.
#' @param cod_change Numerical scalar, vector or matrix.
#' The changes to be applied to the causes of death rate, m[c,x],
#' in order to reduce or increase the mortality estimates given by the life
#' table. Accepted input: any value greater than -100. See examples.
#' @return A life table in the same format as the input life table.
#' @examples
#' 
#' L <- data_gbd2021_lt()  # life tables
#' D <- data_gbd2021_cod() # cod data
#' 
#' # Select Life Table
#' lt <- L[L$region == "Romania" & L$sex == "both" & L$period == 2021, ]
#' # Select COD data
#' cod <- D[D$region == "Romania" & D$sex == "both" & D$period == 2021, ]
#' cod_change = -50
#' 
#' # Example 1:
#' # How does the life table modify if the cause-specific mortality is
#' # reduced by 50% (all ages, all causes of death)?
#' lt_reduced <- modify_life_table(lt, cod, cod_change = -50)
#' lt_reduced
#' 
#' # Example 2:
#' # Let's change the first cod by 1%, second one with 2% and so on until 17%
#' # Note, we are increasing death rates. This should result in a lower life
#' # expectancy.
#' 
#' unique(cod$cause_name) # we have 17 causes
#' 
#' lt_reduced2 <- modify_life_table(lt, cod, cod_change = 1:17)
#' lt_reduced2
#' 
#' # Example 3:
#' # Apply a specific change by cause and age
#' # Say, we want to decrease the cod's risk only between age 45 and 75
#' # with values between 24% and 40%.
#' 
#' # we have to build a matrix with 25 rows and 18 columns (AGEs x CODs)
#' # to indicate the change for each combination
#' M <- matrix(24:40, nrow = 25, ncol = 18, byrow = TRUE)
#' dimnames(M) <- list(unique(cod$x), unique(cod$cause_name))
#' M[!(rownames(M) %in% 45:75), ] <- 0
#' 
#' lt_reduced3 <- modify_life_table(lt, cod, cod_change = -M)
#' lt_reduced3
#' @export
modify_life_table <- function(lt, cod, cod_change) {
  # death counts by cod from a long dataset
  lv  <- as.character(unique(cod$cause_name))
  cod <- build_cod_matrix(cod)[, lv]
  qx  <- lt$qx
  qx[is.na(qx)] <- 0

  # Modify cod matrix by applying a change
  mod_cod <- modify_cod(cod, cod_change)

  # reduced probability of survival by cod
  pxi_r <- (1 - qx) ^ mod_cod

  # all-cause reduced qx
  qx_r <- 1 - apply(pxi_r, 1, prod)

  # Build the life table from qx_r using the standard demographic procedure.
  # The `ax` column from the input table is used directly, which gives a more
  # accurate construction and removes the need for {MortalityLaws}.
  LT <- life_table_from_qx(
    x   = lt$x,
    qx  = qx_r,
    ax  = lt$ax
  )

  # Exit
  # The output should have the same format as the input life table
  out <- lt %>%
    select(
      -all_of(names(LT))
    ) %>%
    bind_cols(LT)

  return(out)
}


#' Modify COD table by changing the cause of death associated risks
#'
#' @inheritParams modify_life_table
#' @return A long table with the same format as the input data
#' @examples
#' D <- data_gbd2021_cod() # cod data
#' 
#' # Select COD data
#' cod <- D[D$region == "Romania" & D$sex == "both" & D$period == 2021, ]
#' cod_change = -50
#' 
#' # Example 1:
#' # Modify all COD values by 50%. This is trivial and not really needed.
#' modify_cod_table(cod, cod_change = -50)
#' 
#' 
#' # Example 2:
#' # Change the first cod by 1%, second one with 2% and so on until 17%
#' modify_cod_table(cod, cod_change = 1:17)
#' 
#' # Example 3:
#' # Apply a specific change by cause and age
#' # Say, we want to decrease the cod's risk only between age 45 and 75
#' # with values between 24% and 40%.
#' 
#' # we have to build a matrix with 25 rows and 18 columns (AGEs x CODs)
#' # to indicate the change for each combination
#' M <- matrix(24:40, nrow = 25, ncol = 18, byrow = TRUE)
#' dimnames(M) <- list(unique(cod$x), unique(cod$cause_name))
#' M[!(rownames(M) %in% 45:75), ] <- 0
#' M
#' 
#' modify_cod_table(cod, cod_change = -M)
#' @export
modify_cod_table <- function(cod, cod_change){

  x = cause_name = deaths = . <- NULL

  #build cod matrix
  cod2 <- cod %>%
    select(x, cause_name, deaths) %>%
    as.data.table() %>%
    # and build a matrix
    dcast(x ~ cause_name, value.var = "deaths") %>%
    as.data.frame() %>%
    arrange(x)  %>%
    # replace na with 0
    mutate_all(~replace(., is.na(.), 0)) %>%
    # name rows
    df_rownames_from("x") %>%
    as.matrix()

  lv <- as.character(unique(cod$cause_name))
  cod2 <- cod2[, lv]

  # Modify cod matrix by applying a change
  mod_cod <- modify_cod(cod2, cod_change)

  # Go from matrix to long table
  out <- mod_cod %>%
    as.data.frame() %>%
    df_add_rownames("x") %>%
    # data.table's melt() only accepts a data.table input (no data.frame
    # method), so coerce before reshaping.
    as.data.table() %>%
    melt(id.vars = "x",
         variable.name = "cause_name",
         value.name = "deaths") %>%
    as.data.frame() %>%
    mutate(
      x = as.numeric(x),
      cause_name = factor(as.character(cause_name), levels = lv)) %>%
    arrange(cause_name)
    # remove original deaths column and join the datasets.
    # NB: drop the column by name, not by index (cod[-6]) -- the latter
    # removes the 6th row on a data.table (data.table[-i] is row subsetting),
    # which silently left a duplicate deaths.x/deaths.y in the output.
  out <- left_join(select(cod, -deaths), out, by = c("x", "cause_name"))

  # Exit
  return(out)
}


#' Format COD data
#'
#' Transform the COD count data from a long table to a matrix
#' containing percentages, with ages as rows and CODs as columns.
#'
#' @param cod COD long table
#' @return A matrix with percentages.
#' @examples
#' # cod data
#' D <- data_gbd2021_cod()
#' # Select COD data for 1 region
#' cod <- D[D$region == "Romania" & D$sex == "both" & D$period == 1990, ]
#' # COD data in matrix format
#' 
#' build_cod_matrix(cod)
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
    # data.table's dcast() only accepts a data.table input; the dplyr verbs
    # above return a tibble, so coerce before reshaping.
    as.data.table() %>%
    # and build a matrix
    dcast(x ~ cause_name, value.var = "perc") %>%
    as.data.frame() %>%
    arrange(x)  %>%
    # replace na with 0
    mutate_all(~replace(., is.na(.), 0)) %>%
    # name rows
    df_rownames_from("x") %>%
    as.matrix()
  # dcast sorts the columns; restore the appearance order of cause_name so the
  # matrix column order matches the input data.
  M <- M[, as.character(unique(cod$cause_name))]

  L <- rowSums(M) < 0.999
  if (any(L)) {
    M[L, ] <- 1/ncol(M)
  }

  return(M)
}


#' Modify COD values by changing the cause of death associated risks
#'
#' @inheritParams modify_life_table
#' @return A matrix with ages as rows and causes of death as columns.
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






