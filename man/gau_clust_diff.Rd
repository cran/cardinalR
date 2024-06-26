% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering.R
\name{gau_clust_diff}
\alias{gau_clust_diff}
\title{Generate Gaussian Clusters with Different Points}
\usage{
gau_clust_diff(
  clust_size_vec,
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
\item{clust_size_vec}{A vector specifying the number of points in each cluster.}

\item{num_clust}{The number of clusters to generate.}

\item{mean_matrix}{A matrix where each row represents the mean vector for a cluster.}

\item{var_vec}{A vector specifying the variance for each cluster.}

\item{num_dims}{The number of effective dimensions for the data points.}

\item{num_noise}{The number of additional noise dimensions to be generated.}

\item{min_n}{The minimum value for the noise added to the data points.}

\item{max_n}{The maximum value for the noise added to the data points.}
}
\value{
A matrix containing the generated Gaussian clusters with different points.
}
\description{
This function generates Gaussian clusters with different numbers of points per cluster.
}
\examples{

# Generate Gaussian clusters with custom parameters
set.seed(20240412)
data <- gau_clust_diff(
  clust_size_vec = c(50, 100, 200, 50),
  num_clust = 4, mean_matrix =
    rbind(
      c(1, 0, 0, 0, 0, 0), c(0, 1, 0, 0, 0, 0),
      c(0, 0, 1, 0, 0, 0), c(0, 0, 0, 1, 0, 0)
    ),
  var_vec = c(0.02, 0.05, 0.06, 0.1),
  num_dims = 6, num_noise = 4,
  min_n = -0.05, max_n = 0.05
)
}
