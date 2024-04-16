## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(cardinalR)
library(langevitour)

## -----------------------------------------------------------------------------
gau_data <- gau_clust(
  n = 500, num_clust = 5,
  mean_matrix = rbind(c(1, 0, 0, 0), c(0, 1, 0, 0), c(0, 0, 1, 0), c(0, 0, 0, 1), c(0, 0, 0, 0)),
  var_vec = c(0.05, 0.05, 0.05, 0.05, 0.05), num_dims = 4, num_noise = 2,
  min_n = -0.05, max_n = 0.05
)

colnames(gau_data) <- paste0("x", seq_len(NCOL(gau_data)))

langevitour(gau_data, pointSize = 2)

## -----------------------------------------------------------------------------
two_grd_data <- two_grid_comb_bkg(n_value = 15, num_noise = 2, min_n = -0.05, max_n = 0.05)
two_grd_data$n

colnames(two_grd_data$df) <- paste0("x", seq_len(NCOL(two_grd_data$df)))

langevitour(two_grd_data$df, pointSize = 2)

## -----------------------------------------------------------------------------
curvy_cyc_data <- curvy_cycle(n = 300, num_noise = 2, min_n = -0.05, max_n = 0.05)

colnames(curvy_cyc_data) <- paste0("x", seq_len(NCOL(curvy_cyc_data)))

langevitour(curvy_cyc_data, pointSize = 2)

## -----------------------------------------------------------------------------
tree_data <- tree(
  n = 490, num_noise = 2,
  min_n = -0.05, max_n = 0.05
)

colnames(tree_data) <- paste0("x", seq_len(NCOL(tree_data)))

langevitour(tree_data, pointSize = 2)

## -----------------------------------------------------------------------------
four_linear_data <- four_long_clust(
  n = 500, num_noise = 2, min_n = -0.05,
  max_n = 0.05
)

colnames(four_linear_data) <- paste0("x", seq_len(NCOL(four_linear_data)))

langevitour(four_linear_data, pointSize = 2)

## -----------------------------------------------------------------------------
two_curvy_panckakes <- two_curvy_panckakes(
  n = 300, num_noise = 2, min_n = -0.05,
  max_n = 0.05
)

colnames(two_curvy_panckakes) <- paste0("x", seq_len(NCOL(two_curvy_panckakes)))

langevitour(two_curvy_panckakes, pointSize = 2)

## -----------------------------------------------------------------------------
sphere_data <- sphere(
  radius = 1, resolution = 20, num_noise = 2,
  min_n = -0.05, max_n = 0.05
)

colnames(sphere_data) <- paste0("x", seq_len(NCOL(sphere_data)))

langevitour(sphere_data, pointSize = 2)

## -----------------------------------------------------------------------------
mobius_clust_data <- mobius_clust(
  n = 500, num_noise = 2, min_n = -0.05,
  max_n = 0.05
)

colnames(mobius_clust_data) <- paste0("x", seq_len(NCOL(mobius_clust_data)))

langevitour(mobius_clust_data, pointSize = 2)

## -----------------------------------------------------------------------------
train_3d_data <- tri_3d(n = 512, num_noise = 2, min_n = -0.05, max_n = 0.05)

colnames(train_3d_data) <- paste0("x", seq_len(NCOL(train_3d_data)))

langevitour(train_3d_data, pointSize = 2)

