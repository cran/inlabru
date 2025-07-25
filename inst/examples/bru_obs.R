\donttest{
if (bru_safe_inla() &&
    require(ggplot2, quietly = TRUE)) {

  # The 'bru_obs()' (previously 'like()') function's main purpose is to set up
  # observation models, both for single- and multi-likelihood models.
  # The following example generates some random covariates which are observed
  # through two different random effect models with different likelihoods

  # Generate the data

  set.seed(123)

  n1 <- 200
  n2 <- 10

  x1 <- runif(n1)
  x2 <- runif(n2)
  z2 <- runif(n2)

  y1 <- rnorm(n1, mean = 2 * x1 + 3)
  y2 <- rpois(n2, lambda = exp(2 * x2 + z2 + 3))

  df1 <- data.frame(y = y1, x = x1)
  df2 <- data.frame(y = y2, x = x2, z = z2)

  # Single likelihood models and inference using bru are done via

  cmp1 <- y ~ -1 + Intercept(1) + x
  fit1 <- bru(cmp1, family = "gaussian", data = df1)
  summary(fit1)

  cmp2 <- y ~ -1 + Intercept(1) + x + z
  fit2 <- bru(cmp2, family = "poisson", data = df2)
  summary(fit2)

  # A joint model has two likelihoods, which are set up using the bru_obs
  # function

  lik1 <- bru_obs(
    "gaussian",
    formula = y ~ x + Intercept,
    data = df1,
    tag = "norm"
  )
  lik2 <- bru_obs(
    "poisson",
    formula = y ~ x + z + Intercept,
    data = df2,
    tag = "pois"
  )

  # The union of effects of both models gives the components needed to run bru

  jcmp <- ~ x + z + Intercept(1)
  jfit <- bru(jcmp, lik1, lik2)

  bru_index(jfit, "norm")
  bru_index(jfit, "pois")

  # Compare the estimates

  p1 <- ggplot() +
    gg(fit1$summary.fixed, bar = TRUE) +
    ylim(0, 4) +
    ggtitle("Model 1")
  p2 <- ggplot() +
    gg(fit2$summary.fixed, bar = TRUE) +
    ylim(0, 4) +
    ggtitle("Model 2")
  pj <- ggplot() +
    gg(jfit$summary.fixed, bar = TRUE) +
    ylim(0, 4) +
    ggtitle("Joint model")

  multiplot(p1, p2, pj)
}
}
