# ------------------------------------------------- #
# Author: Marius D. Pascariu
# ------------------------------------------------- #

# Consolidated function for preparing data across all comparison modes
# (single region, country comparison, sex comparison)

# ----------------------------------------------------------------------------
# PROCESS DATA

#' Prepare data for risk changes and comparisons
#'
#' Prepares cause of death and life table data for analysis across different
#' comparison modes: single region (cod), country comparison (cntr), or
#' sex comparison (sex).
#'
#' @param cod Cause of death table
#' @param lt Life table
#' @param region1 Primary region code
#' @param region2 Secondary region code (same as region1 for single region mode)
#' @param sex Sex category (ignored in sex comparison mode)
#' @param cod_change Numeric vector of risk changes to apply
#' @param mode Comparison mode: "cod" (single region), "cntr" (country comparison),
#'   or "sex" (sex comparison)
#'
#' @return List containing:
#'   - cod_initial: Initial cause of death table
#'   - cod_final: Modified cause of death table (if changes applied)
#'   - lt_initial: Initial life table
#'   - lt_final: Modified life table (if changes applied)
#'
#' @keywords internal
#' @export
prepare_data <- function(cod,
                         lt,
                         region1,
                         region2,
                         sex,
                         cod_change,
                         mode = "cod") {

  # Define filtering logic based on mode
  filter_data <- function(data, region_filter, sex_filter) {
    data[data$region == region_filter & data$sex == sex_filter, ]
  }

  # Initialize tables based on mode
  X <- if (mode == "cod") {
    # Single region mode: Compare before/after risk changes for 1 region
    # If no risk change is applied, the tables before and after are the same
    # (no change in LE, no decomposition)
    list(
      c1 = filter_data(cod, region1, sex),
      c2 = NULL,
      l1 = filter_data(lt, region1, sex),
      l2 = NULL
    )
  } else if (mode == "cntr") {
    # Country comparison mode: Compare 2 regions with optional risk changes
    # We need to adjust the cod and lt tables for both regions if changes are applied
    list(
      c1 = filter_data(cod, region1, sex),
      c2 = filter_data(cod, region2, sex),
      l1 = filter_data(lt, region1, sex),
      l2 = filter_data(lt, region2, sex)
    )
  } else if (mode == "sex") {
    # Sex comparison mode: Compare 2 sexes within 1 region with optional risk changes
    # We need to adjust the cod and lt tables for both sexes if changes are applied
    list(
      c1 = filter_data(cod, region1, "male"),
      c2 = filter_data(cod, region1, "female"),
      l1 = filter_data(lt, region1, "male"),
      l2 = filter_data(lt, region1, "female")
    )
  }

  # Apply risk changes if provided
  has_changes <- any(cod_change != 0)

  if (has_changes) {
    if (mode == "cod") {
      # For single region: modify the final tables only
      X$c2 <- modify_cod_table(X$c1, cod_change)
      X$l2 <- modify_life_table(X$l1, X$c1, cod_change)
    } else {
      # For country/sex comparison: modify both final tables
      X$c1 <- modify_cod_table(X$c1, cod_change)
      X$c2 <- modify_cod_table(X$c2, cod_change)
      X$l1 <- modify_life_table(X$l1, X$c1, cod_change)
      X$l2 <- modify_life_table(X$l2, X$c2, cod_change)
    }
  } else {
    # If no changes, final tables equal initial tables
    if (mode == "cod") {
      X$c2 <- X$c1
      X$l2 <- X$l1
    }
  }

  # Apply factor levels for region in cntr mode
  if (mode == "cntr") {
    lv <- if (region1 == region2) region1 else c(region1, region2)
    X$c1$region <- factor(X$c1$region, levels = lv)
    X$c2$region <- factor(X$c2$region, levels = lv)
  }

  # Return standardized output list
  out <- list(
    cod_initial = X$c1,
    cod_final   = X$c2,
    lt_initial  = X$l1,
    lt_final    = X$l2
  )

  return(out)
}
