# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Thu Jun 10 22:12:12 2021
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
#' @examples 
#' 
#' L <- data_gbd2019_lt  # life tables
#' D <- data_gbd2019_cod # cod data
#' 
#' # Select Life Table
#' lt <- L[L$region == "Romania" & L$sex == "both" & L$level == "median", ]
#' # Select COD data
#' cod <- D[D$region == "Romania" & D$sex == "both" & D$level == "median", ]
#' cod_change = -50
#' 
#' # Example 1:
#' # How does the life table modify if the cause-specific mortality is
#' # reduced by 50%?
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
#' # we have to build a matrix with 24 rows and 17 columns (ages x cods)
#' # to indicate the change for each combination
#' M <- matrix(24:40, nrow = 24, ncol = 17, byrow = TRUE)
#' dimnames(M) <- list(unique(cod$x), unique(cod$cause_name))
#' M[!(rownames(M) %in% 45:75), ] <- 0
#' 
#' lt_reduced3 <- modify_life_table(lt, cod, cod_change = -M)
#' lt_reduced3
#' @export
modify_life_table <- function(lt, cod, cod_change) {
  # death counts by cod from a long dataset
  lv  <- levels(cod$cause_name)
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
#' @examples 
#' D <- data_gbd2019_cod # cod data
#' 
#' # Select COD data
#' cod <- D[D$region == "Romania" & D$sex == "both" & D$level == "median", ]
#' cod_change = -50
#' 
#' # Example 1:
#' # Modify by 50% all COD values. This is trivial and not need really.
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
#' # we have to build a matrix with 24 rows and 17 columns (ages x cods)
#' # to indicate the change for each combination
#' M <- matrix(24:40, nrow = 24, ncol = 17, byrow = TRUE)
#' dimnames(M) <- list(unique(cod$x), unique(cod$cause_name))
#' M[!(rownames(M) %in% 45:75), ] <- 0
#' M
#' 
#' modify_cod_table(cod, cod_change = -M)
#' @export
modify_cod_table <- function(cod, cod_change){
  
  
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
  
  lv <- levels(cod$cause_name)
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
    arrange(cause_name) %>% 
    # remove original deaths column and join the datasets
    left_join(cod[-7], ., by = c("x", "cause_name")) 
  
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
#' @examples 
#' # cod data
#' D <- data_gbd2019_cod 
#' # Select COD data for 1 region
#' cod <- D[D$region == "Romania" & D$sex == "both" & D$level == "median", ]
#' # COD data in matrix format
#' 
#' build_cod_matrix(cod) 
#' @export
build_cod_matrix <- function(cod) {
  
  M <- cod %>%
    # compute percentages of each disease for 
    # given age-region-period-sex and across ages
    group_by(region, period, sex, x, level) %>% 
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
build_reduction_matrix <- function(
  data, 
  select_cod, 
  select_x, 
  cod_change) {
  
  cn <- levels(data$cause_name)
  rn <- unique(data$x)
  
  mat <- matrix(0, 
                ncol = length(cn),
                nrow = length(rn),
                dimnames = list(rn, cn))
  
  select_ages <- rn %in% min(select_x):max(select_x)
  mat[select_ages, select_cod] <- cod_change
  
  return(mat)
}






