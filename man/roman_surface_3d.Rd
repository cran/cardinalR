% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/highd_obj.R
\name{roman_surface_3d}
\alias{roman_surface_3d}
\title{Generate data points on a Roman surface with optional noise.}
\usage{
roman_surface_3d(n, num_noise, min_n, max_n)
}
\arguments{
\item{n}{Total number of data points to generate.}

\item{num_noise}{Number of additional noise dimensions to add to the data.}

\item{min_n}{Minimum value for the noise added to the data.}

\item{max_n}{Maximum value for the noise added to the data.}
}
\value{
A matrix containing the generated data points with or without added noise.
}
\description{
This function generates data points on a Roman surface with optional noise.
}
\examples{
set.seed(20240412)
roman_surface_3d(n = 100, num_noise = 2, min_n = -0.05, max_n = 0.05)

}
