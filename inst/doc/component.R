## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "png",
  dev.args = list(type = "cairo-png"),
  fig.width = 7,
  fig.height = 5
)

## ----results="hide",warning=FALSE,message=FALSE,echo=FALSE----------------------------------------
options(width = 100) # sets width of output window

## ----eval=FALSE-----------------------------------------------------------------------------------
# ~ my_component_name(
#   main = ...,
#   model = ...
# )

## ----eval=FALSE-----------------------------------------------------------------------------------
# ~ my_spde_effect(
#   cbind(x, y),
#   model = spde_model
# )

## ----eval=FALSE-----------------------------------------------------------------------------------
# get_xy <- function(df) {
#   cbind(df$x, df$y)
# }
# ~ my_spde_effect(
#   get_xy(.data.),
#   model = spde_model
# )

## ----eval=FALSE-----------------------------------------------------------------------------------
# ~ my_intercept(1)

## ----eval=FALSE-----------------------------------------------------------------------------------
# ~ my_intercept(
#   main = rep(1, n),
#   model = "linear"
# )

## ----eval=FALSE-----------------------------------------------------------------------------------
# ~ my_sp_effect(
#   main = a_spatial_object,
#   model = "linear"
# )

## ----eval=FALSE-----------------------------------------------------------------------------------
# ~ my_sp_effect(
#   main = eval_spatial(a_spatial_object, .data.),
#   model = "linear"
# )

## ----eval=FALSE-----------------------------------------------------------------------------------
# ~ my_fixed_effects(
#   main = ~ x1:x2 + x3 * x4,
#   model = "fixed"
# )

## ----eval=FALSE-----------------------------------------------------------------------------------
# ~ my_fixed_effects(
#   main = MatrixModels::model.Matrix(~ x1:x2 + x3 * x4, .data.),
#   model = "fixed"
# )

## ----eval=FALSE-----------------------------------------------------------------------------------
# ~ a_component(
#   main = a_function,
#   model = ...
# )

## ----eval=FALSE-----------------------------------------------------------------------------------
# ~ a_component(
#   main = a_function(.data.),
#   model = ...
# )

