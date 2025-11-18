## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(cardinalR)

## -----------------------------------------------------------------------------
# Example: Generate 4D background noise
bkg_data <- gen_bkgnoise(n = 500, p = 4, 
                         m = c(0, 0, 0, 0), s = c(2, 2, 2, 2))
head(bkg_data)

## -----------------------------------------------------------------------------
randomized_data <- randomize_rows(bkg_data)
head(randomized_data)

## -----------------------------------------------------------------------------
df <- tibble::tibble(
  x1 = rnorm(12),
  x2 = rnorm(12),
  x3 = rnorm(12),
  x4 = rnorm(12),
  cluster = rep(1:3, each = 4)
)

vert_mat <- matrix(c(
  5, 0, 0, 0,
  0, 5, 0, 0,
  0, 0, 5, 0
), nrow = 3, byrow = TRUE)

relocated_df <- relocate_clusters(df, vert_mat)
head(relocated_df)

## -----------------------------------------------------------------------------

rotations_4d <- list(
  list(plane = c(1, 2), angle = 60),
  list(plane = c(3, 4), angle = 90)
)

rot_mat <- gen_rotation(p = 4, planes_angles = rotations_4d)
rot_mat

## -----------------------------------------------------------------------------
norm_data <- normalize_data(bkg_data)
head(norm_data)

## -----------------------------------------------------------------------------

centers <- gen_clustloc(p = 4, k = 5)
head(centers)

## ----echo=TRUE----------------------------------------------------------------
gen_nsum(n = 100, k = 3)

## ----echo=TRUE----------------------------------------------------------------
gen_nproduct(n = 500, p = 4)

