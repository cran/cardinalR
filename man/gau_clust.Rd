% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering.R
\name{gau_clust}
\alias{gau_clust}
\title{Generate synthetic data with Gaussian clusters}
\usage{
gau_clust(
  n,
  num_clust,
  mean_matrix,
  var_vec,
  num_dims,
  num_noise,
  min_n,
  max_n
)
}
\arguments{
\item{n}{The total number of data points to be generated.}

\item{num_clust}{The number of clusters to generate.}

\item{mean_matrix}{A matrix where each row represents the mean vector for a cluster.}

\item{var_vec}{A vector specifying the variance for each cluster.}

\item{num_dims}{The number of effective dimensions for the data points.}

\item{num_noise}{The number of additional noise dimensions to be generated.}

\item{min_n}{The minimum value for the noise added to the data points.}

\item{max_n}{The maximum value for the noise added to the data points.}
}
\value{
A matrix containing the generated Gaussian clusters.
}
\description{
Generate Gaussian Clusters
}
\details{
This function generates Gaussian clusters with specified parameters.
}
\examples{

set.seed(20240412)
gau_clust(
  n = 300, num_clust = 5,
  mean_matrix = rbind(
    c(1, 0, 0, 0), c(0, 1, 0, 0), c(0, 0, 1, 0),
    c(0, 0, 0, 1), c(0, 0, 0, 0)
  ), var_vec = c(0.05, 0.05, 0.05, 0.05, 0.05),
  num_dims = 4, num_noise = 2, min_n = -0.05, max_n = 0.05
)
}
