# ------------------------------------------------- #
# Vendored internal life-table construction.
#
# HERE we re-implement the C3_qx construction path of the
# MortalityLaws::LifeTable() function ("life table from age-specific
# probabilities of death") in order to reduce the dependency of our package
# on other packages. Only the input path used by modify_life_table()
# (qx + ax supplied) is reproduced.
# ------------------------------------------------- #


#' Build a life table from age-specific probabilities of death
#'
#' Internal re-implementation of the \code{C3_qx} construction path of the
#' {MortalityLaws} package (\code{LifeTable()}, "Mortality laws method with
#' qx as input"). It returns the same columns and numeric values as
#' \code{MortalityLaws::LifeTable(x, qx, ax)$lt}.
#'
#' @param x Vector of exact ages (start of each age interval).
#' @param qx Age-specific probability of death between ages \code{x} and
#' \code{x + nx}.
#' @param ax Average number of years lived within the age interval by those
#' who die in it. Length 1 (assumed constant across ages) or the same length
#' as \code{x}.
#' @param lx0 Number of survivors at age 0 (radix). Default: 100000.
#'
#' @return A data frame with the standard life-table columns:
#' \code{x.int, x, mx, qx, ax, lx, dx, Lx, Tx, ex}.
#' @keywords internal
#' @noRd
life_table_from_qx <- function(x, qx, ax, lx0 = 1e5) {
  N <- length(x)

  # Width of the age intervals. The last (open-ended) interval is given the
  # same width as the previous one, as in {MortalityLaws}.
  nx <- c(diff(x), diff(x)[N - 1])

  # Central death rates. The last value is extrapolated from the previous
  # two, exactly as in {MortalityLaws}.
  mx <- suppressWarnings(-log(1 - qx) / nx)
  mx[N] <- mx[N - 1]^2 / mx[N - 2]
  mx <- ux_above_100(x, mx)

  # Number of survivors at exact age x
  lx <- lx0 * c(1, cumprod(1 - qx)[1:(N - 1)])

  # Number of deaths in the age interval [x, x + nx)
  dx <- c(-diff(lx), lx[N])

  # Average number of years lived within the age interval by the deceased
  if (length(ax) == 1) {
    ax <- rep(ax, N)
  } else {
    ax <- as.numeric(ax)
  }

  # Person-years lived between x and x + nx
  Lx <- nx * lx - (nx - ax) * dx
  Lx[N] <- ax[N] * dx[N]
  Lx[is.na(Lx)] <- 0

  # Cumulative person-years lived above age x, and life expectancy
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx / lx
  ex[is.na(ex)] <- 0
  ex[N] <- if (ex[N - 1] == 0) 0 else ax[N]

  # Degenerate input guard (all values invalid/zero)
  if (all(is.na(mx)) | all(is.nan(mx)) | all(is.infinite(mx)) | all(mx == 0)) {
    mx <- qx <- ax <- lx <- dx <- Lx <- Tx <- ex <- NA
  }

  data.frame(
    x.int = paste0("[", x, ",", c(x[-1], "+"), ")"),
    x     = x,
    mx    = mx,
    qx    = qx,
    ax    = ax,
    lx    = lx,
    dx    = dx,
    Lx    = Lx,
    Tx    = Tx,
    ex    = ex
  )
}


#' @keywords internal
#' @noRd
ux_above_100 <- function(x, ux) {
  # Replace missing/invalid force-of-mortality values at ages >= 100 with the
  # largest valid value (as {MortalityLaws} does).
  L <- x >= 100 & (is.na(ux) | is.infinite(ux) | ux == 0)
  ux[L] <- max(ux[!L])
  ux
}
