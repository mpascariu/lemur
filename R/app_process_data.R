# -------------------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last Update: Tue Dec 19 22:08:15 2023
# -------------------------------------------------------------- #

# somebody should merge these functions in 1?
  
# ----------------------------------------------------------------------------
# PROCESS DATA

#' Prepare data for risk changes
#' @keywords internal
#' @export
prepare_data_mode_cod <- function(cod,
                                  lt,
                                  region1,
                                  region2,
                                  sex,
                                  cod_change) {

  region <- NULL
  
  # Select cod and lt for 1 region
  # If no risk change is applied the tables before and after are the same
  # not change in LE no decomposition
  c1 <- cod[cod$region == region1 & cod$sex == sex, ]
  c2 <- c1
  l1 <- lt[lt$region == region1  & lt$sex == sex, ]
  l2 <- l1

  # IF there is a change applied we take the initial tables and
  # we modify them
  logic <- any(cod_change != 0)
  if (logic) {
    # c1 <- modify_cod_table(c1, cod_change)
    c2 <- modify_cod_table(c1, cod_change)
    # l1 <- modify_life_table(l1, c1, cod_change)
    l2 <- modify_life_table(l1, c1, cod_change)
  }

  out <- list(
    cod_initial = c1,
    cod_final   = c2,
    lt_initial  = l1,
    lt_final    = l2
    )
  return(out)
}


#' Prepare data for country comparisons
#' @keywords internal
#' @export
prepare_data_mode_cntr <- function(cod,
                                   lt,
                                   region1,
                                   region2,
                                   sex,
                                   cod_change) {
  region <- NULL

  # select cod and lt tables for 2 regions
  c1_init <- c1 <- cod[cod$region == region1 & cod$sex == sex, ]
  c2_init <- c2 <- cod[cod$region == region2 & cod$sex == sex, ]
  l1_init <- l1 <- lt[lt$region == region1 & lt$sex == sex, ]
  l2_init <- l2 <- lt[lt$region == region2 & lt$sex == sex, ]

  # IF we look at 2 regions and we change the risks
  # we need to adjust the cod and lt tables for both regions
  logic <- any(cod_change != 0)
  if (logic) {
    c1 <- modify_cod_table(c1_init, cod_change)
    c2 <- modify_cod_table(c2_init, cod_change)
    l1 <- modify_life_table(l1_init, c1_init, cod_change)
    l2 <- modify_life_table(l2_init, c2_init, cod_change)
  }

  lv <- if (region1 == region2) region1 else c(region1, region2) 
  c1 <- mutate(c1, region = factor(region, levels = lv))
  c2 <- mutate(c2, region = factor(region, levels = lv))
  
  out <- list(
    cod_initial = c1,
    cod_final   = c2,
    lt_initial  = l1,
    lt_final    = l2
  )
  return(out)
}


#' Prepare data for sex comparisons
#' @keywords internal
#' @export
prepare_data_mode_sex <- function(cod,
                                  lt,
                                  region1,
                                  region2,
                                  sex,
                                  cod_change){

  region <- NULL
  sex    <- NULL
  
  # select cod and lt tables for the 2 sexes
  c1_init <- c1 <- cod[cod$region == region1 & cod$sex == "male" , ]
  c2_init <- c2 <- cod[cod$region == region1 & cod$sex == "female", ]
  l1_init <- l1 <- lt[lt$region == region1 & lt$sex == "male", ]
  l2_init <- l2 <- lt[lt$region == region1 & lt$sex == "female", ]

  # IF we look at 2 gender and we change the risks
  # we need to adjust the cod and lt tables for both populations
  logic <- any(cod_change != 0)
  if (logic) {
    c1 <- modify_cod_table(c1_init, cod_change)
    c2 <- modify_cod_table(c2_init, cod_change)
    l1 <- modify_life_table(l1_init, c1_init, cod_change)
    l2 <- modify_life_table(l2_init, c2_init, cod_change)
  }

  out <- list(
    cod_initial = c1,
    cod_final   = c2,
    lt_initial  = l1,
    lt_final    = l2
  )
  return(out)

}


