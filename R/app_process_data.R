# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Mon Nov 22 08:32:23 2021
# --------------------------------------------------- #

# ----------------------------------------------------------------------------
# PROCESS DATA

#' Prepare data for risk changes
#' @keywords internal
#' @export
prepare_data_mode_cod <- function(cod,
                                  lt,
                                  region1,
                                  cod_change) {

  # Select cod and lt for 1 region
  # If no risk change is applied the tables before and after are the same
  # not change in LE no decomposition
  c1 <- cod[cod$region == region1, ]
  c2 <- c1
  l1 <- lt[lt$region == region1, ]
  l2 <- l1

  # IF there is a change applied we take the initial tables and
  # we modify them
  logic <- any(cod_change != 0)
  if(logic) {
    c2 <- modify_cod_table(c1, cod_change)
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
                                   cod_change) {
  region <- NULL

  # select cod and lt tables for 2 regions
  c1 <- cod[cod$region == region1, ]
  c2 <- cod[cod$region == region2, ]
  l1 <- lt[lt$region == region1, ]
  l2 <- lt[lt$region == region2, ]

  # IF we look at 2 regions and we change the risks
  # we need to adjust the cod and lt tables for both regions
  logic <- any(cod_change != 0)
  if(logic) {
    c1 <- modify_cod_table(c1, cod_change)
    c2 <- modify_cod_table(c2, cod_change)
    l1 <- modify_life_table(l1, c1, cod_change)
    l2 <- modify_life_table(l2, c2, cod_change)
  }

  c1 <- mutate(c1, region = factor(region, levels = c(region1, region2)))
  c2 <- mutate(c2, region = factor(region, levels = c(region1, region2)))

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
                                  cod_change){

  # select cod and lt tables for the 2 sexes
  c1 <- cod[cod$sex == "male" & cod$region == region1, ]
  c2 <- cod[cod$sex == "female" & cod$region == region1, ]
  l1 <- lt[lt$sex == "male" & lt$region == region1, ]
  l2 <- lt[lt$sex == "female" & lt$region == region1, ]

  # IF we look at 2 gender and we change the risks
  # we need to adjust the cod and lt tables for both populations
  logic <- any(cod_change != 0)
  if(logic) {
    c1 <- modify_cod_table(c1, cod_change)
    c2 <- modify_cod_table(c2, cod_change)
    l1 <- modify_life_table(l1, c1, cod_change)
    l2 <- modify_life_table(l2, c2, cod_change)
  }

  out <- list(
    cod_initial = c1,
    cod_final   = c2,
    lt_initial  = l1,
    lt_final    = l2
  )
  return(out)

}




