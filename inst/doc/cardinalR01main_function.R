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
rot1 <- gen_rotation(p = 4, planes_angles = list(list(plane = c(1, 2), angle = 60),
                                                list(plane = c(3, 4), angle = 90)))
rot2 <- gen_rotation(p = 4, planes_angles = list(list(plane = c(1, 3), angle = 30)))
rot3 <- gen_rotation(p = 4, planes_angles = list(list(plane = c(2, 4), angle = 45)))


## -----------------------------------------------------------------------------
clust_data <- gen_multicluster(n = c(200, 300, 500), k = 3,
  loc = matrix(c(
    0, 0, 0, 0,
    5, 9, 0, 0,
    3, 4, 10, 7
  ), nrow = 3, byrow = TRUE),
  scale = c(2, 5, 1),
  shape = c("gaussian", "cone", "unifcube"),
  rotation = list(rot1, rot2, rot3),
  add_bkg = FALSE
)

langevitour(clust_data |> dplyr::select(-cluster), 
            pointSize = 2, group = clust_data$cluster)

