## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(cardinalR)
library(langevitour)

## -----------------------------------------------------------------------------
gau_data <- gen_gaussian(n = 500, p = 4, s = diag(4) * 0.01)

langevitour(gau_data, pointSize = 2)

## -----------------------------------------------------------------------------
cube_grd_data <- gen_unifcubehole(n = 5000, p = 6)

langevitour(cube_grd_data, pointSize = 2)

## -----------------------------------------------------------------------------
curvy_cyc_data <- gen_curvycycle(n = 500, p = 4) 

langevitour(curvy_cyc_data, pointSize = 2)

## -----------------------------------------------------------------------------
tree_data <- gen_orgcurvybranches(n = 600, p = 5, k = 6) 

langevitour(tree_data, pointSize = 2)

## -----------------------------------------------------------------------------
cone_data <- gen_cone(n = 500, h = 5, ratio = 0.5) 

langevitour(cone_data, pointSize = 2)

## -----------------------------------------------------------------------------
spiral_data <- gen_conicspiral(n = 500, spins = 2)

langevitour(spiral_data, pointSize = 2)

## -----------------------------------------------------------------------------
sphere_data <- gen_gridedsphere(n = 500, p = 4)

langevitour(sphere_data, pointSize = 2)

## -----------------------------------------------------------------------------
scurve_data <- gen_scurvehole(n = 600) 

langevitour(scurve_data, pointSize = 2)

## -----------------------------------------------------------------------------
pyr_data <- gen_pyrfrac(n = 1000, p = 4)

langevitour(pyr_data, pointSize = 2)

