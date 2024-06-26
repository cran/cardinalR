% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linear.R
\name{four_long_clust_bkg}
\alias{four_long_clust_bkg}
\title{Generate Four Long Clusters with Background Noise}
\usage{
four_long_clust_bkg(n, num_noise, min_n, max_n)
}
\arguments{
\item{n}{The total number of data points to be generated.}

\item{num_noise}{The number of additional noise dimensions to be generated.}

\item{min_n}{The minimum value for the noise added to the data points.}

\item{max_n}{The maximum value for the noise added to the data points.}
}
\value{
A matrix containing the generated data, with each row representing a data point.
}
\description{
This function generates data with four long clusters along with background noise.
}
\examples{
# Generate four long clusters with background noise with custom parameters
set.seed(20240412)
data <- four_long_clust_bkg(
  n = 400, num_noise = 4, min_n = -0.05,
  max_n = 0.05
)
}
