% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering.R
\name{gau_curvy_clust}
\alias{gau_curvy_clust}
\title{Generate Cluster and Curvilinear Data with Noise}
\usage{
gau_curvy_clust(n, clust_size_vec, num_noise, min_n, max_n)
}
\arguments{
\item{n}{The total number of data points to be generated.}

\item{clust_size_vec}{A vector specifying the number of points for each cluster.
If not provided, the n is divided equally
between the two clusters.}

\item{num_noise}{The number of additional noise dimensions to be generated.}

\item{min_n}{The minimum value for the noise added to the data points.}

\item{max_n}{The maximum value for the noise added to the data points.}
}
\value{
A matrix containing the generated data, with each row representing a data point.
}
\description{
This function generates data with two clusters, one following a curvilinear
pattern and the other distributed randomly.
}
\examples{

# Generate cluster and curvilinear data with custom parameters
set.seed(20240412)
data <- gau_curvy_clust(
  n = 300, clust_size_vec = c(100, 200), num_noise = 3,
  min_n = -0.05, max_n = 0.05
)
}
