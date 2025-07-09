## ----include = FALSE------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(inlabru)
inla_available <- inlabru::bru_safe_inla(
  multicore = interactive(),
  quietly = TRUE
)

## ----setup, include=TRUE, eval=inla_available-----------------------------------------------------
library(magrittr)
library(dplyr)
library(scoringRules)

## ----eval=inla_available--------------------------------------------------------------------------
df_pois <- tibble::tibble(
  x = rnorm(50),
  y = rpois(length(x), exp(2 + 1 * x))
)
fit_pois <- bru(
  y ~ Intercept(1) + x,
  family = "poisson",
  data = df_pois
)
newdata_pois <- tibble::tibble(
  x = rnorm(100),
  y = rpois(100, exp(2 + 1 * x))
)

## ----eval=inla_available--------------------------------------------------------------------------
pred_pois <- predict(
  fit_pois,
  newdata_pois,
  formula = ~ exp(Intercept + x),
  n.samples = 2000
)
post_E <- pred_pois$mean
post_Var <- pred_pois$mean + pred_pois$sd^2
scores <- data.frame(
  SE = (newdata_pois$y - post_E)^2,
  DS = (newdata_pois$y - post_E)^2 / post_Var + log(post_Var)
)
summary(scores)

## ----eval=inla_available--------------------------------------------------------------------------
pred_pois <- predict(fit_pois,
  newdata_pois,
  formula = ~ dpois(y, lambda = exp(Intercept + x)),
  n.samples = 2000
)
log_score <- -log(pred_pois$mean)
summary(log_score)

## ----eval=inla_available--------------------------------------------------------------------------
head(
  data.frame(
    log_S = log_score,
    lower = -log(pred_pois$mean + 2 * pred_pois$mean.mc_std_err),
    upper = -log(pred_pois$mean - 2 * pred_pois$mean.mc_std_err)
  )
)

## ----eval=inla_available--------------------------------------------------------------------------
# some large value, so that 1-F(K) is small
max_K <- ceiling(max(newdata_pois$y) + 4 * sqrt(max(newdata_pois$y)))
k <- seq(0, max_K)
kk <- rep(k, times = length(newdata_pois$y))
i <- seq_along(newdata_pois$y)
pred_pois <- generate(fit_pois, newdata_pois,
  formula = ~ {
    lambda <- exp(Intercept + x)
    ppois(kk, lambda = rep(lambda, each = length(k)))
  },
  n.samples = 2000
)
results <- data.frame(
  i = rep(i, each = length(k)),
  k = kk,
  Fpred = rowMeans(pred_pois),
  residuals =
    rowMeans(pred_pois) - (rep(newdata_pois$y, each = length(k)) <= kk)
)
# Check that the cutoff point K has nearly probability mass 1 below it,
# for all i:
min(results %>% dplyr::filter(k == max_K) %>% pull(Fpred))

crps_scores <-
  (results %>%
    group_by(i) %>%
    summarise(crps = sum(residuals^2), .groups = "drop") %>%
    pull(crps))
summary(crps_scores)

## ----eval=inla_available--------------------------------------------------------------------------
true_sigma_y <- 0.1
df_normal <- tibble::tibble(
  x = rnorm(50),
  y = rnorm(length(x), 2 + 1 * x, sd = true_sigma_y)
)
fit_normal <- bru(
  y ~ Intercept(1) + x,
  family = "normal",
  data = df_normal
)
newdata_normal <- tibble::tibble(
  x = rnorm(100),
  y = rnorm(100, 2 + 1 * x, sd = true_sigma_y)
)

## ----eval=inla_available--------------------------------------------------------------------------
pred_normal <- predict(fit_normal, newdata_normal,
  formula = ~ {
    E <- Intercept + x
    V <- 1 / Precision_for_the_Gaussian_observations
    list(
      eta = E,
      sigma_y_2 = V,
      dens = dnorm(y, mean = E, sd = sqrt(V))
    )
  },
  n.samples = 2000
)
post_E <- pred_normal$eta$mean
post_Var <- pred_normal$sigma_y_2$mean + pred_normal$eta$sd^2
scores_normal <- data.frame(
  SE = (newdata_normal$y - post_E)^2,
  DS = (newdata_normal$y - post_E)^2 / post_Var + log(post_Var),
  LS = -log(pred_normal$dens$mean)
)
summary(scores_normal)

## ----eval=inla_available--------------------------------------------------------------------------
head(
  data.frame(
    log_S = scores_normal$LS,
    lower = -log(pred_normal$dens$mean + 2 * pred_normal$dens$mean.mc_std_err),
    upper = -log(pred_normal$dens$mean - 2 * pred_normal$dens$mean.mc_std_err)
  )
)

## ----eval=inla_available--------------------------------------------------------------------------
pred_pois <- predict(fit_pois, newdata_pois, formula = ~ exp(Intercept + x))
scores <- (newdata_pois$y - pred_pois$mean)^2

## ----eval=inla_available--------------------------------------------------------------------------
pred_pois <- predict(fit_pois, newdata_pois,
  formula = ~ {
    lambda <- exp(Intercept + x)
    list(
      cond_scores = (y - lambda)^2,
      lambda = lambda
    )
  },
  n.samples = 2000
)
scores <- pred_pois$cond_scores$mean - pred_pois$lambda$sd^2
summary(scores)

## ----eval=inla_available--------------------------------------------------------------------------
max_K <- 100 # some large value, so that 1-F(K) is small
pred_pois <- predict(fit_pois, newdata_pois,
  formula = ~ {
    lambda <- exp(Intercept + x)
    list(
      crps = scoringRules::crps_pois(y, lambda),
      F = do.call(
        c,
        lapply(
          seq_along(y),
          function(i) {
            ppois(seq(0, max_K), lambda = lambda[i])
          }
        )
      )
    )
  },
  n.samples = 2000
)
crps_score <-
  pred_pois$crps$mean -
  (pred_pois$F %>%
    mutate(i = rep(seq_along(newdata_pois$y), each = max_K + 1)) %>%
    group_by(i) %>%
    summarise(F_var = sum(sd^2), .groups = "drop") %>%
    pull(F_var))
summary(crps_score)

