% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering.R
\name{one_doublet_bkg}
\alias{one_doublet_bkg}
\title{Generate Doublets with Background Noise}
\usage{
one_doublet_bkg(n, num_noise, min_n, max_n)
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
This function generates data with doublets (pairs of clusters) along with
added background noise.
}
\examples{

# Generate doublets with background noise with custom parameters
set.seed(20240412)
data <- one_doublet_bkg(n = 250, num_noise = 2, min_n = -0.05, max_n = 0.05)
}
