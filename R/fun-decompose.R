# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed Nov 17 17:46:34 2021
# --------------------------------------------------- #

#' Perform decomposition of age-specific mortality contributions
#' in life expectancy between any two regions/time periods
#'
#' Decompose the difference in life expectancy (at the smallest available age)
#' of two populations represented by their life tables in a given
#' period of time.
#'
#' @inheritParams decompose_by_cod
#' @return A numerical vector.
#' @references ...
#' @examples
#' # Data
#' L <- data_gbd2019_lt
#'
#' # Select Life Table 1 & 2
#' region1 = "ROMANIA"
#' region2 = "MEXICO"
#' sex     = "female"
#' year    = 2019
#'
#' L1 <- L[L$region == region1 & L$sex == sex & L$period == year, ]
#' L2 <- L[L$region == region2 & L$sex == sex & L$period == year, ]
#'
#' # Age decomposition
#' dec <- decompose_by_age(L1, L2)
#' dec
#'
#' plot_decompose(dec)
#' @export
decompose_by_age <- function(L1, L2){

  region = period = sex = x = x.int <- NULL

  l <- L1$lx
  L <- L1$Lx

  dx <- L2$dx
  lx <- L2$lx
  Lx <- L2$Lx
  Tx <- L2$Tx

  LAG <- dim(L1)[1]  # Last Age Group

  # Compute the direct and indirect components
  DE <- (l/l[1]) * (Lx/lx - L/l)
  IE <- Tx[-1]/lx[1] * (l[-LAG]/lx[-LAG] - l[-1]/lx[-1])

  # one extra value for the indirect component
  # since there is only direct component in the last age group
  IE <- c(IE, 0)

  ## add both to get the overall age-decomposition
  dec <- DE + IE

  # Format and add identification columns from LT data
  out <- L1 %>%
    select(
      region,
      period,
      sex,
      x.int,
      x
    ) %>%
    mutate(
      x.int = factor(x.int, levels = unique(x.int)),
      decomposition = dec,
      region = ifelse(
        L1$region == L2$region,
        region,
        paste(L1$region, "-", L2$region)),
      period = ifelse(
        L1$period == L2$period,
        period,
        paste(L1$period, "-", L2$period)),
      sex = ifelse(
        L1$sex == L2$sex,
        sex,
        paste(L1$sex, "-", L2$sex)),
    )

  # Add a new S3 class and exit
  out <- new_tibble(out, nrow = nrow(out), class = "decompose")
  return(out)
}


#' Perform decomposition of age- and cause-specific mortality contributions
#' in life expectancy between any two regions/time periods
#'
#' @param L1 Life table corresponding to the region/time period 1;
#' @param L2 Life table corresponding to the region/time period 2;
#' @param C1 Causes of death data containing death counts corresponding
#' to the region/time period 1. Format: long table;
#' @param C2 Causes of death data containing death counts corresponding
#' to the region/time period 2. Format: long table;
#' @param symmetrical logical. default TRUE as recommended by authors. Shall
#'  we average the results of replacing 1 with 2 and 2 with 1?
#'  The symmetrical argument toggles whether or not we replace cod1 with cod2
#' (FALSE), or take the arithmetic average or replacement in both directions.
#' Defaults are set to symmetrically replace from the bottom up,
#' per the authors' suggestion.
#' @param direction character. One of "up", "down", or "both". Default "up",
#' as recommended by authors. It refers to whether we go from the bottom up or
#' top down, or take the arithmetic average of these when replacing vector
#' elements. Although the total difference will always sum correctly,
#' the calculated contribution from individual components can vary greatly
#' depending on the order in general.
#' @details This implements the algorithm described in Andreev et al (2002),
#' with defaults set to approximate their recommendations for replacement
#' ordering and result averaging.
#' @return A long tibble in the same format as the cod input data. The output
#' containS the column \code{decomposition} that indicates the change in life
#' expectancy by age and/or cause of death between the two life tables provided
#' as input. Measure unit: years.
#' @source The code of this function is based on the implementation of the
#' \code{DemoDecomp::stepwise_replacement} function maintained by Tim RIFFE.
#' @references ...
#' @examples
#' L <- data_gbd2019_lt  # life tables
#' D <- data_gbd2019_cod # cod data
#'
#' # Select two Life Tables
#' region1 = "ROMANIA"
#' region2 = "MEXICO"
#' sex     = "male"
#' year    = 2019
#'
#' lt1 <- L[L$region == region1 & L$sex == sex & L$period == year, ]
#' lt2 <- L[L$region == region2 & L$sex == sex & L$period == year, ]
#'
#' # Select COD corresponding data
#' cod1 <- D[D$region == region1 & D$sex == sex & D$period == year, ]
#' cod2 <- D[D$region == region2 & D$sex == sex & D$period == year, ]
#'
#' ## Example of decomposition by age and cause of death
#' dec  <- decompose_by_cod(L1 = lt1,
#'                          L2 = lt2,
#'                          C1 = cod1,
#'                          C2 = cod2)
#'
#' dec
#'
#' plot_decompose(dec)
#' @export
decompose_by_cod <- function(L1,
                             L2,
                             C1,
                             C2,
                             symmetrical = TRUE,
                             direction = "up"){


  direction <- tolower(direction)
  stopifnot(direction %in% c("up", "down", "both"))
  up    <- direction %in% c("up","both")
  down  <- direction %in% c("down","both")
  pars1 <- build_cod_matrix(C1) * L1$mx
  pars2 <- build_cod_matrix(C2) * L2$mx

  x        <- L1$x
  N        <- length(c(pars1))
  pars1Mat <- matrix(c(pars1), ncol = N + 1, nrow = N)
  pars2Mat <- matrix(c(pars2), ncol = N + 1, nrow = N)
  R        <- matrix(ncol = N + 1, nrow = N)

  # based on 1-> 2 upward
  r1ind <- lower.tri(pars1Mat, TRUE)
  r2ind <- upper.tri(pars1Mat)
  dec   <- matrix(NA, nrow = N, ncol = 4)

  if (up){
    R[r1ind] <- pars1Mat[r1ind]
    R[r2ind] <- pars2Mat[r2ind]
    dec[, 1] <- diff(apply(R, 2, FUN = exFUN, x = x))
  }

  if (down) {
    R[r1ind[N:1, ]] <- pars1Mat[r1ind[N:1, ]]
    R[r2ind[N:1, ]] <- pars2Mat[r2ind[N:1, ]]
    dec[, 2] <- rev(diff(apply(R, 2, FUN = exFUN, x = x)))
  }

  if (symmetrical){
    if (up){
      R[r1ind] <- pars2Mat[r1ind]
      R[r2ind] <- pars1Mat[r2ind]
      dec[, 3] <- -diff(apply(R, 2, FUN = exFUN, x = x))
    }

    if (down){
      R[r1ind[N:1, ]] <- pars2Mat[r1ind[N:1, ]]
      R[r2ind[N:1, ]] <- pars1Mat[r2ind[N:1, ]]
      dec[, 4] <- rev(-diff(apply(R, 2, FUN = exFUN, x = x)))
    }
  }
  # take the mean
  dec <- rowMeans(dec, na.rm = TRUE)

  # Fine tune the results before exit to
  # fall exactly on the life expectancy gap of the two life tables
  dec <- dec * (L2$ex[1] - L1$ex[1])/sum(dec, na.rm = TRUE)

  # rename the dimensions of the matrix
  dim(dec) <- c(length(x), length(dec)/length(x))
  dimnames(dec) <- dimnames(pars1)

  # Create a nice long table before exit
  out <- matrix_to_long_table(dec, C1, C2)

  # Add a new S3 class and exit
  out <- new_tibble(out, nrow = nrow(out), class = "decompose")
  return(out)
}


#' Life expectancy function
#' @param x vector of ages
#' @param cod Matrix containing cause-specific death rates
#' @return life expectancy at birth (or min age)
#' @keywords internal
exFUN <- function(x, cod){
  dim(cod) <- c(length(x),length(cod)/length(x))

  mx <- rowSums(cod)
  nx <- c(diff(x), Inf)
  px <- exp(-mx*{{nx}})
  qx <- 1-px
  lx <- cumprod(c(1, px))[seq_along(px)]
  dx <- c(-diff(lx), rev(lx)[1])
  Lx <- ifelse(mx == 0, lx * nx, dx/mx)
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx/lx

  return(ex[1])
}


#'Format matrix into a long table
#'and add the identification columns from cod data
#'@param X Matrix with cod decomposition data;
#'@inheritParams decompose_by_cod
#'@keywords internal
matrix_to_long_table <- function(X, C1, C2){

  region = period = x = x.int = cause_name = decomposition <- NULL
  sex = sex1 = sex2 = r1 = r2 = p1 = p2 <- NULL

  X %>%
    as.data.frame() %>%
    rownames_to_column(var = "x") %>%
    pivot_longer(
      cols = -x,
      names_to = "cause_name",
      values_to = "decomposition") %>%
    # add columns with info from cod tables
    # if info differs in the two cod tables we merge them
    mutate(
      x      = as.numeric(x),
      x.int  = as.character(cut(x, breaks = c(unique(x), Inf), right = FALSE)),
      x.int  = ifelse(x.int == "[110,Inf)", "[110,+)", x.int),
      x.int  = factor(x.int, levels = unique(x.int)),
      cause_name = factor(cause_name, levels = levels(C1$cause_name)),

      r1     = unique(C1$region),
      r2     = unique(C2$region),
      region = ifelse(r1 == r2, r1, paste(r1, "-", r2)),

      p1     = unique(C1$period),
      p2     = unique(C2$period),
      period = ifelse(p1 == p2, p1, paste(p1, "-", p2)),

      sex1   = unique(C1$sex),
      sex2   = unique(C2$sex),
      sex    = ifelse(sex1 == sex2, sex1, paste(sex1, "-", sex2)),
    ) %>%
    select(
      region,
      period,
      sex,
      x.int,
      x,
      cause_name,
      decomposition) %>%
    arrange(cause_name, x)

}





