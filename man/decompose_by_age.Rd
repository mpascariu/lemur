% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun-decompose.R
\name{decompose_by_age}
\alias{decompose_by_age}
\title{Perform decomposition of age-specific mortality contributions
in life expectancy between any two regions/time periods}
\usage{
decompose_by_age(L1, L2)
}
\arguments{
\item{L1}{Life table corresponding to the region/time period 1;}

\item{L2}{Life table corresponding to the region/time period 2;}
}
\value{
A numerical vector.
}
\description{
Decompose the difference in life expectancy (at the smallest available age)
of two populations represented by their life tables in a given
period of time.
}
\examples{
# Data
L <- data_gbd2021_lt

# Select Life Table 1 & 2
region1 = "Romania"
region2 = "Mexico"
sex     = "female"
year    = 2021

L1 <- L[L$region == region1 & L$sex == sex & L$period == year, ]
L2 <- L[L$region == region2 & L$sex == sex & L$period == year, ]

# Age decomposition
dec <- decompose_by_age(L1, L2)
dec

plot_decompose(dec)
}
\references{
...
}
