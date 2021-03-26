# --------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Fri Mar 26 18:10:49 2021
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
#' @param cod Causes of death matrix corresponding to the population and 
#' time period of the life table;
#' @param cod_change The change to be applied to the causes of death risks
#' in order to reduce of increase the mortality estimates given by the life 
#' table.
#' @examples 
#' L <- data_gbd2019_lt  # life tables
#' D <- data_gbd2019_cod # cod data
#' 
#' # Select Life Table
#' lt <- L[L$region == "Romania" & L$sex == "both" & L$level == "median", ]
#' # Select COD data
#' cod <- D[D$region == "Romania" & D$sex == "both" & D$level == "median", ]
#' 
#' # How does the life table modify if the cause-specific mortality is
#' # reduced by 50%?
#' lt_reduced <- reduce_LifeTable(lt, cod, cod_change = 50)
#' lt_reduced
#' @export
reduce_LifeTable <- function(lt, cod, cod_change) {
  
  cod <- build_cod_matrix(cod)
  # Reduction
  r <- 1 - cod_change / 100
  
  # Compute competing risks prob. and apply the reduction/change
  # WORKS FOR VECTOR INPUTS TOO. TO BE TESTED FOR MATRICES!!!
  qx  <- replace_na(lt$qx, 0)
  qxi <- 1 - (1 - qx) ^ t(t(cod) * r) 
  
  # Convert qx's to lx's
  lxi <- convertFx(
    x    = lt$x,
    data = qxi,
    from = "qx",
    to   = "lx",
    lx0  = 1)
  
  # Compute non-COD lx 
  lx <- apply(lxi, 1, prod)
  
  # Build life-table from lx using standard procedure from {MortalityLaws}
  sex <- lt$sex[1]
  sex <- ifelse(lt$sex[1] == "both", "total", lt$sex[1])
  
  LT <- LifeTable(
    x   = lt$x,
    lx  = lx,
    ax  = lt$ax,
    sex = sex
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
#' cod's as columns. Also replace na's with a very small number to avoid errors.
#' 
#' @param y COD long table
#' @param vsn very small scalar to replace na values
#' @keywords internal
build_cod_matrix <- function(y, vsn = 0) {
  y %>%
    select(x, cause_name, perc) %>% 
    pivot_wider(
      names_from = cause_name,
      values_from = perc
    ) %>% 
    arrange(x) %>% 
    mutate_all(~replace(., is.na(.), vsn)) %>%
    column_to_rownames("x")
}







