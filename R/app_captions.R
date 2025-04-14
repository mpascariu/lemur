# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Mon Apr 14 23:49:23 2025
# ------------------------------------------------- #


# ALL THE INFORMATIVE CAPTIONS FOR FIGURES AND TABLES ARE CODED HERE
# WE TRY TO MAKE THEM AS DYNAMIC AS POSSIBLE IN ORDER TO BE INFORMATIVE 
# GIVEN THE SHINY SELECTION
# 
# BY HAVING A DEDICATED FILE FOR THIS WE KEEP THE SERVER SCRIPT SHORT AND 
# AS READABLE AS POSSIBLE

#' Generate captions for data tables
#' All arguments are shiny inputs
#' @keywords internal
generate_table_captions <- function(mode,
                                    region1, 
                                    region2,
                                    time_slider, 
                                    sex, 
                                    cod_change) {
  
  # Compose captions for our tables:
  # We have 2 lifetables, 2 COD distribution tables and 1 decomposition table
  # Depending on what selection is performed on the dashboard we would need
  # to adjust the information in the caption of each table so that we 
  # keep the app intuitive.
  # 
  lt_initial = "NO CAPTION FOR THIS TABLE GIVEN THE CURRENT SELECTION. SUGGEST ONE, PLEASE!"
  lt_final = cod_initial = cod_final  = decomposition <- lt_initial
  
  part1 <- paste0(
    ifelse(sex == "both", "total ", sex), 
    " population, ", region1, " (", time_slider, ")"
  )
  part1.2 <- paste0(
    ifelse(sex == "both", "total ", sex), 
    " population, ", region2, " (", time_slider, ")"
  )
  
  part2 <- paste0(
    "The values in the table are not affected by changes ",
    "in mortality risks as applied in the dashboard."
  )
  
  part3 <- ifelse(
    cod_change != 0, 
    paste0(
      "The values are resulted from a change of ", 
      cod_change, "% applied to one or more risks factors ",
      "as specified in the dashboard."), 
    "")
  
  part4 <- paste0(
    "The values are resulted from various changes to the ", 
    "SDG accomplishment levels applied to one or more risks factors ",
    "as specified in the dashboard.") 
  
  if (mode %in% c("mode_cod", "mode_sdg", "mode_sdg2")) {
    lt_initial <- paste0(
      "TABLE 1 -- Life table for ", part1, ". ", part2,
      " TABLE 2 below includes such alterations."
    )
    
    lt_final <- paste0(
      "TABLE 2 -- Hypothetical life table for ", part1, ". ", part3
    )
    cod_initial <- paste0(
      "TABLE 3 -- Distribution of death counts by cause for ", part1, ". ", part2,
      " TABLE 4 below includes such alterations."
    )
    cod_final <- paste0(
      "TABLE 4 -- Hypothetical distribution of deaths by cause for ", 
      part1, ". ", part3
    )
    decomposition <- paste0(
      "TABLE 5 -- Differences in life expectancy at birth attributable to ",
      "each cause of death for ", part1, ". ", part3
    )
  }
  
  if (mode %in% c("mode_sdg", "mode_sdg2")) {
    
    lt_final <- paste0(
      "TABLE 2 -- Hypothetical life table for ", part1, ". ", part4
    )
    cod_final <- paste0(
      "TABLE 4 -- Hypothetical distribution of death counts by cause for ", 
      part1, ". ", part4
    )
    decomposition <- paste0(
      "TABLE 5 -- Differences in life expectancy at birth attributable to ",
      "each cause of death for ", part1, ". ", part4
    )
  }
  
  if (mode == "mode_cntr") {
    lt_initial <- paste0(
      "TABLE 1 -- Life table for ", part1, ". ", part3
    )
    lt_final <- paste0(
      "TABLE 2 -- Life table for ", part1.2, ". ", part3
    )
    cod_initial <- paste0(
      "TABLE 3 -- Distribution of deaths by cause for ", part1, ". ", part3
    )
    cod_final <- paste0(
      "TABLE 4 -- Distribution of deaths by cause for ", part1.2, ". ", part3
    )
    decomposition <- paste0(
      "TABLE 5 -- Differences in life expectancy at birth attributable to ",
      "each cause of death between ", region1, " and ", region2,
      " in ", time_slider, ". ",
      ifelse(cod_change != 0, paste0("In addition ", tolower(part3)), "")
    )
  }
  
  if (mode == "mode_sex") {
    lt_initial <- paste0(
      "TABLE 1 -- Life table for male population, ", 
      region1, " (", time_slider, "). ", part3
    )
    lt_final <- paste0(
      "TABLE 2 -- Life table for female population, ", 
      region1, " (", time_slider, "). ", part3
    )
    cod_initial <- paste0(
      "TABLE 3 -- Distribution of deaths by cause for male population, ",
      region1, " (", time_slider, "). ", part3
    )
    cod_final <- paste0(
      "TABLE 4 -- Distribution of deaths by cause for female population, ", 
      region1, " (", time_slider, "). ", part3
    )
    decomposition <- paste0(
      "TABLE 5 -- Sex-gap in life expectancy at birth attributable to ",
      "each cause of death, ", 
      region1, " (", time_slider, "). ",
      ifelse(cod_change != 0, paste0("In addition ", tolower(part3)), "")
    )
  }
  
  
  reduction_matrix <- paste0(
    "TABLE 6 -- Matrix displaying the applied % changes in mortality by age group and cause of death"
  )
  
  captions <- c(lt_initial, lt_final, cod_initial, cod_final, decomposition,
                reduction_matrix)
  return(captions)
}

#' Generate Fig2 captions
#' All arguments are shiny inputs except the last 2 life tables
#' @keywords internal
generate_fig2_captions <- function(mode,
                                   region1,
                                   region2,
                                   fig2_x,
                                   perc,
                                   cod_change,
                                   cod_target,
                                   lt_initial, 
                                   lt_final
                                   ) {
  
  # Build a dynamic x-axis title to help with interpretability
  # Part 1 - absolute or relative values?
  x_min      <- min(as.numeric(fig2_x))
  x_min_text <- if (x_min == 0) " at birth" else paste(" at age", x_min)
  l0         <- lt_initial
  l1         <- lt_final
  
  
  suffix <- if (cod_change == 0) " in life expectancy" else ""
  xlab_part1 <- ifelse(perc, 
    paste0("\nRelative difference", suffix),
    paste0("\nDifference", suffix)
  )
  
  # Part 2 - increase, decrease how much, where? 
  xlab_part2 <- if (mode %in% c("mode_sdg", "mode_sdg2")) {
    " following the applied changes in mortality risks"
    
    } else if(all(!is.null(cod_target)) & cod_change != 0) {
    paste0(
      " following a ", abs(cod_change), "%",
      ifelse(sign(cod_change) == -1, " reduction", " increase"),
      " in ",
      ifelse(length(cod_target) == 1,
             paste(cod_target, "related deaths"),
             "multiple causes of death")
    )
    
  } else {
    ""
  }
  
  # Part 3 - Life expectancy before and after
  if (mode %in% c("mode_cod", "mode_sdg", "mode_sdg2")) {
    prefix1 <- "Before: "
    prefix2 <- "After the changes: "
  }
  if (mode == "mode_sex") {
    prefix1 <- "Males: "
    prefix2 <- "Females: "
  }
  if (mode == "mode_cntr") {
    prefix1 <- paste0(region1, ": ")
    prefix2 <- paste0(region2, ": ")
  }
  
  xlab_part3 <- paste0(
    "\n[",
    prefix1, 
    round(l0$ex[l0$x == x_min], 2), 
    " vs. ", 
    prefix2, 
    round(l1$ex[l1$x == x_min], 2),
    " years", x_min_text, "]"
  )
  
  out <- paste0(xlab_part1, xlab_part2, xlab_part3)
  return(out)
}


#' Generate Fig3 captions
#' All arguments are shiny inputs
#' @keywords internal
generate_fig3_captions <- function(perc) {
  
  xlab <- if (perc) {
    "Proportion of the Total No. of Deaths\n[%]"
  } else {
    "Number of Deaths\n"
  }
  
  xlab
}


#' Generate Fig4 captions
#' All arguments are shiny inputs
#' @keywords internal
generate_fig4_captions <- function(perc, fig4_dim) {
  
  # compute % is necessary
  if (perc & fig4_dim != "cod") {
    ttip <- c("fill", "y")
    ylab <- "Change in Life Expectancy at Birth\n[%]"
    xlab <- "Age Group (years)"
    
  } else if (!perc & fig4_dim != "cod"){
    ttip <- c("fill", "y")
    ylab <- "Change in Life Expectancy at Birth\n(years)"
    xlab <- "Age Group (years)"
    
  } else if (perc & fig4_dim == "cod"){
    ttip = c("fill", "x")
    ylab <- "Causes of Death"
    xlab <- "Change in Life Expectancy at Birth [%]"
    
  } else if (!perc & fig4_dim == "cod"){
    ttip = c("fill", "x")
    ylab <- "Causes of Death"
    xlab <- "Change in Life Expectancy at Birth [years]"
  }
  
  out <- list(xlab = xlab,
              ylab = ylab,
              ttip = ttip)
  return(out)
}























