# --------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Sat Apr 03 15:42:43 2021
# --------------------------------------------------- #

#' Perform decomposition of age-specific mortality contributions 
#' in life expectancy between any two populations
#' 
#' Decompose the difference in life expectancy (at the smallest available age) 
#' of two populations represented by their life tables in a given 
#' period of time.
#'  
#' @param A Life table 1.
#' @param B Life table 2.
#' @return A numerical vector.
#' @author ...
#' @references ...
#' @examples 
#' # Data
#' X <- data_gbd2019_lt
#' 
#' # Select Life Table 1 & 2
#' L1 <- X[X$region == "Romania" & X$sex == "both" & X$level == "median", ]
#' L2 <- X[X$region == "Republic of Moldova" & X$sex == "both" & X$level == "median", ]
#' 
#' # Life expectancy at birth difference
#' L2$ex[1] - L1$ex[1] # about 2 years diff
#' 
#' # Age decomposition
#' decompose_by_age(L1, L2)
#' @export
decompose_by_age <- function(A, B){
  
  l <- A$lx
  L <- A$Lx
  
  dx <- B$dx
  lx <- B$lx
  Lx <- B$Lx
  Tx <- B$Tx
  
  LAG <- dim(A)[1]  # Last Age Group
  
  # Compute the direct and indirect components
  DE <- (l/l[1]) * (Lx/lx - L/l)
  IE <- Tx[-1]/lx[1] * (l[-LAG]/lx[-LAG] - l[-1]/lx[-1])
  
  # one extra value for the indirect component
  # since there is only direct component in the last age group
  IE <- c(IE, 0)
  
  ## add both to get the overall age-decomposition
  out <- DE + IE
  names(out) <- A$x
  
  # exit
  return(out)
}


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
#' lt_reduced <- cause_modify_life_table(lt, cod, cod_change = -50)
#' lt_reduced
#' 
#' # Example 2:
#' # Let's change the first cod by 1%, second one with 2% and so on until 17%
#' # Note, we are increasing death rates. This should result in a lower life 
#' # expectancy.
#' 
#' unique(cod$cause_name) # we have 17 causes
#' 
#' lt_reduced2 <- cause_modify_life_table(lt, cod, cod_change = 1:17)
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
#' lt_reduced3 <- cause_modify_life_table(lt, cod, cod_change = -M)
#' lt_reduced3
#' @export
cause_modify_life_table <- function(lt, cod, cod_change) {
  
  cod <- build_cod_matrix(cod)    # death counts by cod from a long dataset
  r   <- 1 + cod_change / 100      # Reduction
  qx  <- replace_na(lt$qx, 0)
  
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
    pxi_r <- (1 - qx) ^ (cod * r)
    
  } else {
    # is cod_change is a vector then we have to transpose to 
    # do the multiplication correctly 
    pxi_r <- (1 - qx) ^ t(t(cod) * r)
    
  }
  
  qx_r <- 1 - apply(pxi_r, 1, prod)  # all-cause reduced qx
  
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



#' Transform the COD data from a long table to a matrix with ages as rows and 
#' cod's as columns. 
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
    column_to_rownames("x")
  
  return(M)  
}



