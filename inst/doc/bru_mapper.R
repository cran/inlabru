## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup, eval = TRUE, echo=FALSE, include=FALSE----------------------------
library(inlabru)

## ----eval=FALSE---------------------------------------------------------------
# ?bru_mapper # Mapper constructors, with links to predefined mappers
# ?bru_mapper_generics # Generic and default methods
# ?bru_get_mapper # Mapper extraction methods

## ----eval=FALSE---------------------------------------------------------------
# ibm_eval(mapper, input, state)

## ----eval=FALSE---------------------------------------------------------------
# mapper <- bm_scale()
# ibm_eval(mapper,
#   input = ...,
#   state = ...
# )

## ----eval=FALSE---------------------------------------------------------------
# mapper <- bm_marginal(
#   qfun = ..., pfun = ..., dfun = ..., ..., inverse = ...
# )
# ibm_eval(mapper,
#   input = NULL, # If a list is given, it overrides the parameter specification
#   state = ...
# )
# 
# # Examples:
# mapper <- bm_marginal(qfun = qexp, rate = 1 / 8)
# mapper <- bm_marginal(
#   qfun = qexp, pfun = pexp, dfun = dexp, rate = 1 / 8
# )

## ----eval=FALSE---------------------------------------------------------------
# mapper <- bm_aggregate(rescale = ...)
# ibm_eval(mapper,
#   input = list(block = ..., weights = ...),
#   state = ...
# )

## ----eval=FALSE---------------------------------------------------------------
# mapper <- bm_logsumexp(rescale = ...)
# ibm_eval(mapper,
#   input = list(block = ..., weights = ...),
#   state = ...
# )

## ----eval=FALSE---------------------------------------------------------------
# mapper <- bm_collect(list(name1 = ..., name2 = ..., ...),
#   hidden = FALSE
# )
# ibm_eval(mapper,
#   input = list(name1 = ..., name2 = ..., ...),
#   state = ...
# )
# # If hidden = TRUE, the inla_f=TRUE argument "hides" all but the first mapper:
# ibm_eval(mapper,
#   input = name1_input,
#   state = ...,
#   inla_f = TRUE
# )

## ----eval=FALSE---------------------------------------------------------------
# mapper <- bm_multi(list(name1 = ..., name2 = ..., ...))
# ibm_eval(mapper,
#   input = list(name1 = ..., name2 = ..., ...),
#   state = ...
# )

## ----eval=FALSE---------------------------------------------------------------
# mapper <- bm_pipe(list(name1 = ..., name2 = ..., ...))
# ibm_eval(mapper,
#   input = list(name1 = ..., name2 = ..., ...),
#   state = ...
# )

## ----eval=FALSE---------------------------------------------------------------
# mapper <- bm_sum(mappers = ..., single_input = ...)
# ibm_eval(mapper,
#   input = ...,
#   state = ...
# )

## ----eval=FALSE---------------------------------------------------------------
# mapper <- bm_repeat(mapper = ..., n_rep = ..., interleaved = ...)
# ibm_eval(mapper,
#   input = ...,
#   state = ...
# )

## ----eval=FALSE---------------------------------------------------------------
# mapper <-
#   bm_pipe(
#     list(
#       mapper = bm_multi(list(main = ..., group = ..., replicate = ...)),
#       scale = bm_scale()
#     )
#   )
# ibm_eval(mapper,
#   input = list(
#     mapper = list(main = ..., group = ..., replicate = ...),
#     scale = ...
#   ),
#   state = ...
# )

## ----eval=FALSE---------------------------------------------------------------
# bru_get_mapper.my_unique_model_class <- function(model, ...) {
#   ...
# }

## ----eval=FALSE---------------------------------------------------------------
# ibm_n(mapper, inla_f, ...)
# ibm_n_output(mapper, input, ...)
# ibm_values(mapper, inla_f, ...)
# ibm_jacobian(mapper, input, state, ...)
# ibm_eval(mapper, input, state, ...)
# ibm_names(mapper, ...)
# ibm_inla_subset(mapper, ...)

## ----eval=TRUE----------------------------------------------------------------
mapper <- bm_collect(
  list(
    a = bm_index(3),
    b = bm_index(2)
  ),
  hidden = TRUE
)
ibm_n(mapper)
ibm_values(mapper)
ibm_n(mapper, inla_f = TRUE)
ibm_values(mapper, inla_f = TRUE)

## ----eval=TRUE----------------------------------------------------------------
ibm_n(mapper, multi = TRUE)
ibm_values(mapper, multi = TRUE)
ibm_n(mapper, inla_f = TRUE, multi = TRUE)
ibm_values(mapper, inla_f = TRUE, multi = TRUE)

## ----eval=TRUE----------------------------------------------------------------
mapper <- bm_multi(list(
  a = bm_index(3),
  b = bm_index(2)
))
ibm_n(mapper)
ibm_n(mapper, multi = TRUE)
ibm_values(mapper)
ibm_values(mapper, multi = TRUE)

## ----eval=TRUE----------------------------------------------------------------
ibm_n(mapper, inla_f = TRUE)
ibm_n(mapper, multi = TRUE, inla_f = TRUE)
ibm_values(mapper, inla_f = TRUE)
ibm_values(mapper, multi = TRUE, inla_f = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# bru_mapper(mesh)

## ----eval=FALSE---------------------------------------------------------------
# # If ibm_values() should return mesh$loc (e.g. for "rw2" models
# # with degree=1 meshes)
# bru_mapper(mesh, indexed = FALSE)
# # If ibm_values() should return seq_along(mesh$loc) (e.g. for
# # inla.spde2.pcmatern() models)
# bru_mapper(mesh, indexed = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# bru_mapper_define(mapper, new_class)

## ----eval=FALSE---------------------------------------------------------------
# bru_mapper_define(
#   mapper = list(n = 10, values = 1:10),
#   new_class = "my_bm_class_name"
# )

## ----eval=TRUE----------------------------------------------------------------
bm_p_quadratic <- function(labels, min_degree = 0, ...) {
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
  bru_mapper_define(mapper, new_class = "bm_p_quadratic")
}

## ----eval=TRUE----------------------------------------------------------------
ibm_n.bm_p_quadratic <- function(mapper, ...) {
  p <- length(mapper$labels)
  (mapper$min_degree <= 0) + (mapper$min_degree <= 1) * p + p * (p + 1) / 2
}

## ----eval=TRUE----------------------------------------------------------------
ibm_values.bm_p_quadratic <- function(mapper, ...) {
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

## ----eval=FALSE---------------------------------------------------------------
# bm_p_quadratic <- function(labels, min_degree = 0, ...) {
#   ...
#   mapper <- bru_mapper_define(mapper, new_class = "bm_p_quadratic")
#   mapper$n <- ibm_n_bm_p_quadratic(mapper)
#   mapper$values <- ibm_values_bm_p_quadratic(mapper)
#   mapper
# }

## ----eval=TRUE----------------------------------------------------------------
ibm_jacobian.bm_p_quadratic <- function(mapper, input, ...) {
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

