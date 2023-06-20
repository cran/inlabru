## ---- include = FALSE-----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-------------------------------------------------------------------------
library(inlabru)

## ----eval=FALSE-----------------------------------------------------------------------------------
#  pred <- predict(fit, newdata, formula = ~ exp(eta), n.samples = 2000)
#  post_E <- pred$mean
#  post_Var <- pred$mean + pred$sd^2
#  SE_score <- (newdata$y - post_E)^2
#  DS_score <- (newdata$y - post_E)^2 / post_Var + log(post_Var)

## ----eval=FALSE-----------------------------------------------------------------------------------
#  pred <- predict(fit,
#    newdata,
#    formula = ~ dpois(y, rate = exp(eta)),
#    n.samples = 2000
#  )
#  log_score <- log(pred$mean)

## ----eval=FALSE-----------------------------------------------------------------------------------
#  # some large value, so that 1-F(K) is small
#  max_K <- ceiling(max(y)) + 4 * sqrt(max(y))
#  pred <- generate(fit, newdata,
#    formula = ~ {
#      lambda <- exp(eta)
#      k <- seq(0, max_K)
#      do.call(
#        cbind,
#        lapply(
#          seq_along(y),
#          function(i) {
#            Fpred <- ppois(k, rate = lambda[i])
#            data.frame(
#              k = c(k, k),
#              i = c(i, i),
#              type = rep(c("F", "residual"), each = length(Fpred)),
#              value = c(Fpred, Fpred - (y[i] <= k))
#            )
#          }
#        )
#      )
#    },
#    n.samples = 2000
#  )
#  F_estimate <-
#    (pred %>%
#      filter(type == "F") %>%
#      group_by(i) %>%
#      summarise(F = sum(mean), groups = "drop") %>%
#      pull("F"))
#  crps_score <-
#    (pred %>%
#      filter(type == "residual") %>%
#      group_by(i) %>%
#      summarise(crps = sum(mean^2), groups = "drop") %>%
#      pull(crps))
#  # Check that the cutoff point K has nearly probability mass 1 below it,
#  # for all i:
#  min(F_estimate)

## ----eval=FALSE-----------------------------------------------------------------------------------
#  pred <- predict(fit, newdata, formula = ~ exp(eta))
#  scores <- (y - pred$mean)^2

## ----eval=FALSE-----------------------------------------------------------------------------------
#  pred <- predict(fit, newdata, formula = ~ list(
#    cond_scores = (y - exp(eta))^2,
#    lambda = exp(eta)
#  ))
#  scores <- pred$cond_scores$mean - pred$lambda$sd^2

## ----eval=FALSE-----------------------------------------------------------------------------------
#  poisson_crps <- function(y, rate) {
#    # compute the CRPS score for a single y, for the given rate paramter.
#  }
#  max_K <- 100 # some large value, so that 1-F(K) is small
#  pred <- predict(fit, newdata,
#    formula = ~ {
#      lambda <- exp(eta)
#      list(
#        crps = vapply(
#          seq_along(y),
#          function(i) poisson_crps(y[i], lambda[i]),
#          0.0
#        ),
#        F = do.call(
#          cbind,
#          lapply(
#            seq_along(y),
#            function(i) {
#              data.frame(
#                i = i,
#                F = ppois(seq(0, max_K), rate = lambda[i])
#              )
#            }
#          )
#        )
#      )
#    },
#    n.samples = 2000
#  )
#  crps_score <-
#    pred$crsp$mean -
#    (pred$F %>%
#      group_by(i) %>%
#      summarise(F_var = sum(sd^2), groups = "drop") %>%
#      pull(F_var))

