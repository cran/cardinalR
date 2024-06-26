% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sphere.R
\name{diff_sphere}
\alias{diff_sphere}
\title{Generate data representing small spheres within a larger encompassing sphere with added noise.}
\usage{
diff_sphere(n, num_noise, min_n, max_n)
}
\arguments{
\item{n}{Total number of data points to generate, should be a multiple of 13.}

\item{num_noise}{Number of additional noise dimensions to add to the data.}

\item{min_n}{Minimum value for the noise added to the data.}

\item{max_n}{Maximum value for the noise added to the data.}
}
\value{
A matrix containing the generated data points with or without added noise.
}
\description{
This function generates data points representing small spheres within a larger encompassing sphere
and adds noise to the data if specified.
}
\examples{
set.seed(20240412)
diff_sphere(
  n = 390, num_noise = 2,
  min_n = -0.05, max_n = 0.05
)

}
