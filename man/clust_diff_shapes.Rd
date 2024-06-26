% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering.R
\name{clust_diff_shapes}
\alias{clust_diff_shapes}
\title{Generate Clusters with Different Shapes}
\usage{
clust_diff_shapes(
  n,
  num_gau_clust,
  num_non_gau_clust,
  clust_sd_gau,
  clust_sd_non_gau,
  num_dims,
  a,
  b
)
}
\arguments{
\item{n}{The total number of data points to be generated.}

\item{num_gau_clust}{The number of Gaussian clusters to generate.}

\item{num_non_gau_clust}{The number of non-Gaussian clusters to generate.}

\item{clust_sd_gau}{The standard deviation for the Gaussian clusters.}

\item{clust_sd_non_gau}{The standard deviation for the non-Gaussian clusters.}

\item{num_dims}{The number of dimensions for the data points.}

\item{a}{The scaling factor for the non-Gaussian cluster shape.}

\item{b}{The translation factor for the non-Gaussian cluster shape.}
}
\value{
A matrix containing the generated clusters with different shapes.
}
\description{
This function generates clusters with different shapes, including both Gaussian
and non-Gaussian clusters.
}
\examples{
# Generate clusters with default parameters
set.seed(20240412)
data <- clust_diff_shapes(
  n = 300, num_gau_clust = 4,
  num_non_gau_clust = 2, clust_sd_gau = 0.05, clust_sd_non_gau = 0.1,
  num_dims = 7, a = 2, b = 4
)
}
