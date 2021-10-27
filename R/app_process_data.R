# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed Oct 27 23:35:12 2021
# --------------------------------------------------- #

# ----------------------------------------------------------------------------
# PROCESS DATA

#' Prepare data for risk changes
#' @keywords internal
prepare_data_mode_cod <- function(cod, 
                                  lt, 
                                  region1, 
                                  region2, 
                                  cod_target, 
                                  cod_change) {
  
  # Select cod and lt for 1 region
  # If no risk change is applied the tables before and after are the same
  # not change in LE no decomposition
  c1 <- dplyr::filter(cod, region == region1)
  c2 <- c1
  l1 <- dplyr::filter(lt, region == region1)
  l2 <- l1
  
  # IF there is a change applied we take the initial tables and
  # we modify them
  logic <- any(cod_change != 0) & !is.null(cod_target)
  if(logic) {
    c2 <- modify_cod_table(c1, cod_change)
    l2 <- modify_life_table(l1, c1, cod_change)
  }
  
  out <- list(cod1 = c1, cod2 = c2, lt1 = l1, lt2 = l2)
  return(out)
} 


#' Prepare data for country comparisons
#' @keywords internal
prepare_data_mode_cntr <- function(cod, 
                                   lt, 
                                   region1, 
                                   region2, 
                                   cod_target, 
                                   cod_change) {
  
  # select cod and lt tables for 2 regions
  c1 <- dplyr::filter(cod, region == region1)
  c2 <- dplyr::filter(cod, region == region2)
  l1 <- dplyr::filter(lt, region == region1)
  l2 <- dplyr::filter(lt, region == region2)
  
  # IF we look at 2 regions and we change the risks 
  # we need to adjust the cod and lt tables for both regions
  logic <- any(cod_change != 0) & !is.null(cod_target)
  if(logic) {
    c1 <- modify_cod_table(c1, cod_change)
    c2 <- modify_cod_table(c2, cod_change)
    l1 <- modify_life_table(l1, c1, cod_change)
    l2 <- modify_life_table(l2, c2, cod_change)
  }
  
  c1 <- mutate(c1, region = factor(region, levels = c(region1, region2)))
  c2 <- mutate(c2, region = factor(region, levels = c(region1, region2)))
  
  out <- list(cod1 = c1, cod2 = c2, lt1 = l1, lt2 = l2)
  return(out)
}


#' Prepare data for sex comparisons
#' @keywords internal
prepare_data_mode_sex <- function(region1, 
                                  cod_target, 
                                  cod_change,
                                  year){
  
  cod <- MortalityCauses::data_gbd2019_cod %>% 
    dplyr::filter(
      region == region1,
      period == year,
    )
  
  lt <- MortalityCauses::data_gbd2019_lt %>% 
    dplyr::filter(
      region == region1,
      period == year,
    )
  
  # select cod and lt tables for the 2 sexes
  c1 <- dplyr::filter(cod, sex == "male")
  c2 <- dplyr::filter(cod, sex == "female")
  l1 <- dplyr::filter(lt, sex == "male")
  l2 <- dplyr::filter(lt, sex == "female")
  
  # IF we look at 2 gender and we change the risks 
  # we need to adjust the cod and lt tables for both populations
  logic <- any(cod_change != 0) & !is.null(cod_target)
  if(logic) {
    c1 <- modify_cod_table(c1, cod_change)
    c2 <- modify_cod_table(c2, cod_change)
    l1 <- modify_life_table(l1, c1, cod_change)
    l2 <- modify_life_table(l2, c2, cod_change)
  }
  
  out <- list(cod1 = c1, cod2 = c2, lt1 = l1, lt2 = l2)
  return(out)
  
}
