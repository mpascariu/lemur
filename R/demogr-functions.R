# --------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Wed Mar 24 19:01:01 2021
# --------------------------------------------------- #

#' Perform decomposition of age-specific mortality contributions 
#' in life expectancy between any two populations
#' 
#' Decompose the difference in life expectancy (at the smallest age) 
#' of two populations represented by their life tables in a given 
#' period of time.
#'  
#' @param A Life table 1.
#' @param B Life table 2.
#' @author ...
#' @references ...
#' @examples 
#' library(MortalityCauses)
#' library(dplyr)
#' 
#' # Select Life Table 1
#' L1 <- data_gbd2019_lt %>% 
#'   filter(region == "Romania",
#'          sex == "both",
#'          level == "median")
#' 
#' # Life Table 2
#' L2 <- data_gbd2019_lt %>% 
#'   filter(region == "Republic of Moldova",
#'          sex == "both",
#'          level == "median")
#' 
#' # Life expectancy at birth difference
#' delta_ex <- L2$ex[1] - L1$ex[1] # about 2 years diff
#' 
#' # Age decomposition
#' delta_ex_by_age <- decompose_ex_by_age(L1, L2)
#' @export
decompose_ex_by_age <- function(A, B){
  
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




















