% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grid.R
\name{two_grid_comb}
\alias{two_grid_comb}
\title{Generate One Grid with Different Offset}
\usage{
two_grid_comb(n_value, num_noise, min_n, max_n)
}
\arguments{
\item{n_value}{The number of grid points along each axis for the grids.}

\item{num_noise}{The number of additional noise dimensions to add to the data.}

\item{min_n}{The minimum value for the noise dimensions.}

\item{max_n}{The maximum value for the noise dimensions.}
}
\value{
A list containing the grid dataset with different offsets and the sample size.
}
\description{
This function generates a single grid dataset with a different offset.
}
\examples{
set.seed(20240412)
two_grid_comb <- two_grid_comb(
  n_value = 10, num_noise = 2, min_n = -0.05,
  max_n = 0.05
)
}
