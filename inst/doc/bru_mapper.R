## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(inlabru)

## ----eval=FALSE---------------------------------------------------------------
#  ?bru_mapper # Mapper constructors
#  ?bru_mapper_methods # Mapper methods
#  ?bru_get_mapper # Mapper extraction methods

## ----eval=FALSE---------------------------------------------------------------
#  ibm_n(mapper, inla_f, ...)
#  ibm_values(mapper, inla_f, ...)
#  ibm_jacobian(mapper, input, ...)
#  ibm_inla_subset(mapper, ...)
#  ibm_valid_input(mapper, input, ...)

## ----eval=TRUE----------------------------------------------------------------
mapper <- bru_mapper_collect(
  list(
    a = bru_mapper_index(3),
    b = bru_mapper_index(2)
  ),
  hidden = TRUE
)
ibm_n(mapper)
ibm_values(mapper)
ibm_n(mapper, inla_f = TRUE)
ibm_values(mapper, inla_f = TRUE)

## ----eval=TRUE----------------------------------------------------------------
mapper <- bru_mapper_multi(list(
  a = bru_mapper_index(3),
  b = bru_mapper_index(2)
))
ibm_n(mapper)
ibm_n(mapper, multi = 1)
ibm_values(mapper)
ibm_values(mapper, multi = 1)

## ----eval=FALSE---------------------------------------------------------------
#  bru_mapper(mesh)

## ----eval=FALSE---------------------------------------------------------------
#  # If ibm_values() should return mesh$loc (e.g. for "rw2" models with degree=1 meshes)
#  bru_mapper(mesh, indexed = FALSE)
#  # If ibm_values() should return seq_along(mesh$loc) (e.g. for inla.spde2.pcmatern() models)
#  bru_mapper(mesh, indexed = TRUE)

## -----------------------------------------------------------------------------
bru_mapper_p_quadratic <- function(labels, min_degree = 0, ...) {
  if (is.factor(labels)) {
    mapper <- list(
      labels = levels(labels),
      min_degree = min_degree
    )
  } else {
    mapper <- list(
      labels = as.character(labels),
      min_degree = min_degree
    )
  }
  bru_mapper_define(mapper, new_class = "bru_mapper_p_quadratic")
}

## -----------------------------------------------------------------------------
ibm_n.bru_mapper_p_quadratic <- function(mapper, ...) {
  p <- length(mapper$labels)
  (mapper$min_degree <= 0) + (mapper$min_degree <= 1) * p + p * (p + 1) / 2
}

## -----------------------------------------------------------------------------
ibm_values.bru_mapper_p_quadratic <- function(mapper, ...) {
  p <- length(mapper$labels)
  n <- ibm_n(mapper)
  jk <- expand.grid(seq_len(p), seq_len(p))
  jk <- jk[jk[, 2] <= jk[, 1], , drop = FALSE]
  c(
    if (mapper$min_degree <= 0) "Intercept" else NULL,
    if (mapper$min_degree <= 1) mapper$labels else NULL,
    paste0(mapper$labels[jk[, 1]], ":", mapper$labels[jk[, 2]])
  )
}

## -----------------------------------------------------------------------------
bru_mapper_p_quadratic <- function(labels, min_degree = 0, ...) {
  ...
  mapper <- bru_mapper_define(mapper, new_class = "bru_mapper_p_quadratic")
  mapper$n <- ibm_n_bru_mapper_p_quadratic(mapper)
  mapper$values <- ibm_values_bru_mapper_p_quadratic(mapper)
  mapper
}

## -----------------------------------------------------------------------------
ibm_jacobian.bru_mapper_p_quadratic <- function(mapper, input, ...) {
  if (is.null(input)) {
    return(Matrix::Matrix(0, 0, ibm_n(mapper)))
  }
  p <- length(mapper$labels)
  n <- ibm_n(mapper)
  N <- NROW(input)
  A <- list()
  in_ <- as(input, "Matrix")
  idx <- 0
  if (mapper$min_degree <= 0) {
    idx <- idx + 1
    A[[idx]] <- Matrix::Matrix(1, N)
  }
  if (mapper$min_degree <= 1) {
    idx <- idx + 1
    A[[idx]] <- in_
  }
  for (k in seq_len(p)) {
    idx <- idx + 1
    A[[idx]] <- in_[, seq(k, p, by = 1), drop = FALSE] * in_[, k]
    A[[idx]][, k] <- A[[idx]][, k] / 2
  }
  A <- do.call(cbind, A)
  colnames(A) <- as.character(ibm_values(mapper))
  A
}

