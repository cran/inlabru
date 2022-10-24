## ---- include = FALSE-----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "png",
  dev.args = list(type = "cairo-png"),
  fig.width = 7,
  fig.height = 5
)

## ----setup, include = FALSE-----------------------------------------------------------------------
library(inlabru)
library(ggplot2)

## -------------------------------------------------------------------------------------------------
lambda <- function(u, gamma) {
  -pnorm(u, lower.tail = FALSE, log.p = TRUE) / gamma
}
lambda_inv <- function(lambda, gamma) {
  qnorm(-lambda * gamma, lower.tail = FALSE, log.p = TRUE)
}
D1lambda <- function(u, gamma) {
  exp(dnorm(u, log = TRUE) - pnorm(u, lower.tail = FALSE, log.p = TRUE)) / gamma
}
D2lambda <- function(u, gamma) {
  D1L <- D1lambda(u, gamma)
  -D1L * (u - gamma * D1L)
}
D1log_lambda <- function(u, gamma) {
  D1lambda(u, gamma) / lambda(u, gamma)
}
D2log_lambda <- function(u, gamma) {
  D1logL <- D1log_lambda(u, gamma)
  -D1logL * (u - gamma * D1lambda(u, gamma = gamma) + D1logL)
}

## -------------------------------------------------------------------------------------------------
log_p <- function(u, y, gamma) {
  L <- lambda(u, gamma)
  n <- length(y)
  dnorm(u, log = TRUE) - n * L + n * mean(y) * log(L) - sum(lgamma(y + 1))
}
D1log_p <- function(u, y, gamma) {
  n <- length(y)
  -u + n * D1log_lambda(u, gamma) * (mean(y) - lambda(u, gamma))
}
D2log_p <- function(u, y, gamma) {
  n <- length(y)
  -1 +
    n * D2log_lambda(u, gamma) * (mean(y) - lambda(u, gamma)) -
    n * D1log_lambda(u, gamma) * D1lambda(u, gamma)
}

## -------------------------------------------------------------------------------------------------
g <- 1
y <- c(0, 1, 2)
y <- c(0, 0, 0, 0, 0)
y <- rpois(5, 5)
mu_quad <- uniroot(
  D1log_p,
  lambda_inv((1 + sum(y)) / (g + length(y)) * c(1 / 10, 10), gamma = g),
  y = y, gamma = g
)$root
sd_quad <- (-D2log_p(mu_quad, y = y, gamma = g))^-0.5
sd_lin <- (1 + length(y) * D1log_lambda(mu_quad, gamma = g)^2 * lambda(mu_quad, gamma = g))^-0.5
lambda0 <- lambda(mu_quad, gamma = g)

## -------------------------------------------------------------------------------------------------
ggplot() +
  xlim(
    lambda(mu_quad - 4 * sd_quad, gamma = g),
    lambda(mu_quad + 4 * sd_quad, gamma = g)
  ) +
  xlab("lambda") +
  ylab("Density") +
  geom_function(
    fun = function(x) {
      exp(log_p(lambda_inv(x, gamma = g), y = y, gamma = g)) /
        D1lambda(lambda_inv(x, gamma = g), gamma = g) / (
          exp(log_p(lambda_inv(lambda0, gamma = g), y = y, gamma = g)) /
            D1lambda(lambda_inv(lambda0, gamma = g), gamma = g)
        ) *
        dgamma(lambda0, shape = 1 + sum(y), rate = g + length(y))
    },
    mapping = aes(col = "Theory"),
    n = 1000
  ) +
  geom_function(
    fun = dgamma,
    args = list(shape = 1 + sum(y), rate = g + length(y)),
    mapping = aes(col = "Theory"),
    n = 1000
  ) +
  geom_function(
    fun = function(x) {
      dnorm(lambda_inv(x, gamma = g), mean = mu_quad, sd = sd_quad) /
        D1lambda(lambda_inv(x, gamma = g), gamma = g)
    },
    mapping = aes(col = "Quadratic"),
    n = 1000
  ) +
  geom_function(
    fun = function(x) {
      dnorm(lambda_inv(x, gamma = g), mean = mu_quad, sd = sd_lin) /
        D1lambda(lambda_inv(x, gamma = g), gamma = g)
    },
    mapping = aes(col = "Linearised"),
    n = 1000
  ) +
  geom_vline(mapping = aes(
    xintercept = (1 + sum(y)) / (g + length(y)),
    lty = "Bayes mean"
  )) +
  geom_vline(mapping = aes(xintercept = lambda0, lty = "Bayes mode")) +
  geom_vline(mapping = aes(xintercept = mean(y), lty = "Plain mean"))

## -------------------------------------------------------------------------------------------------
ggplot() +
  xlim(
    lambda_inv(lambda0, gamma = g) - 4 * sd_quad,
    lambda_inv(lambda0, gamma = g) + 4 * sd_quad
  ) +
  xlab("u") +
  ylab("Density") +
  geom_function(
    fun = function(x) {
      exp(log_p(x, y = y, gamma = g) -
        log_p(lambda_inv(lambda0, gamma = g), y = y, gamma = g)) *
        (dgamma(lambda0, shape = 1 + sum(y), rate = g + length(y)) *
          D1lambda(lambda_inv(lambda0, gamma = g), gamma = g))
    },
    mapping = aes(col = "Theory"),
    n = 1000
  ) +
  geom_function(
    fun = function(x) {
      dgamma(lambda(x, gamma = g), shape = 1 + sum(y), rate = g + length(y)) *
        D1lambda(x, gamma = g)
    },
    mapping = aes(col = "Theory"),
    n = 1000
  ) +
  geom_function(
    fun = dnorm,
    args = list(mean = mu_quad, sd = sd_quad),
    mapping = aes(col = "Quadratic"),
    n = 1000
  ) +
  geom_function(
    fun = dnorm,
    args = list(mean = mu_quad, sd = sd_lin),
    mapping = aes(col = "Linearised"),
    n = 1000
  ) +
  geom_vline(mapping = aes(xintercept = lambda_inv((1 + sum(y)) / (g + length(y)),
    gamma = g
  ), lty = "Bayes mean")) +
  geom_vline(mapping = aes(xintercept = lambda_inv(lambda0, gamma = g), lty = "Bayes mode")) +
  geom_vline(mapping = aes(xintercept = lambda_inv(mean(y), gamma = g), lty = "Plain mean"))

## -------------------------------------------------------------------------------------------------
ggplot() +
  xlim(
    lambda(mu_quad - 4 * sd_quad, gamma = g),
    lambda(mu_quad + 4 * sd_quad, gamma = g)
  ) +
  xlab("lambda") +
  ylab("CDF") +
  geom_function(
    fun = pgamma,
    args = list(shape = 1 + sum(y), rate = g + length(y)),
    mapping = aes(col = "Theory"),
    n = 1000
  ) +
  geom_function(
    fun = function(x) {
      pnorm(lambda_inv(x, gamma = g), mean = mu_quad, sd = sd_quad)
    },
    mapping = aes(col = "Quadratic"),
    n = 1000
  ) +
  geom_function(
    fun = function(x) {
      pnorm(lambda_inv(x, gamma = g), mean = mu_quad, sd = sd_lin)
    },
    mapping = aes(col = "Linearised"),
    n = 1000
  ) +
  geom_vline(mapping = aes(
    xintercept = (1 + sum(y)) / (g + length(y)),
    lty = "Bayes mean"
  )) +
  geom_vline(mapping = aes(xintercept = lambda0, lty = "Bayes mode")) +
  geom_vline(mapping = aes(xintercept = mean(y), lty = "Plain mean"))

## -------------------------------------------------------------------------------------------------
ggplot() +
  xlim(
    lambda_inv(lambda0, gamma = g) - 4 * sd_quad,
    lambda_inv(lambda0, gamma = g) + 4 * sd_quad
  ) +
  xlab("u") +
  ylab("CDF") +
  geom_function(
    fun = function(x) {
      pgamma(lambda(x, gamma = g), shape = 1 + sum(y), rate = g + length(y))
    },
    mapping = aes(col = "Theory"),
    n = 1000
  ) +
  geom_function(
    fun = pnorm,
    args = list(mean = mu_quad, sd = sd_quad),
    mapping = aes(col = "Quadratic"),
    n = 1000
  ) +
  geom_function(
    fun = pnorm,
    args = list(mean = mu_quad, sd = sd_lin),
    mapping = aes(col = "Linearised"),
    n = 1000
  ) +
  geom_vline(mapping = aes(
    xintercept = lambda_inv((1 + sum(y)) / (g + length(y)),
      gamma = g
    ),
    lty = "Bayes mean"
  )) +
  geom_vline(mapping = aes(
    xintercept = lambda_inv(lambda0, gamma = g),
    lty = "Bayes mode"
  )) +
  geom_vline(mapping = aes(
    xintercept = lambda_inv(mean(y), gamma = g),
    lty = "Plain mean"
  ))

