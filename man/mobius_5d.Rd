% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mobius_clust.R
\name{mobius_5d}
\alias{mobius_5d}
\title{Generate a 5-D Mobius Strip}
\usage{
mobius_5d(n, num_noise, min_n, max_n)
}
\arguments{
\item{n}{The number of points to generate for the Mobius strip.}

\item{num_noise}{The number of additional noise dimensions to add to the data.}

\item{min_n}{The minimum value for the noise dimensions.}

\item{max_n}{The maximum value for the noise dimensions.}
}
\value{
A matrix containing the generated Mobius strip.
}
\description{
This function generates a dataset representing a 5-dimensional Mobius strip.
}
\examples{
set.seed(20240412)
mobius_data <- mobius_5d(n = 100, num_noise = 2, min_n = -0.05, max_n = 0.05)
}
