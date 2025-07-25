#' @title Matern correlation or covariance function approximate credible bands.
#'
#' @description Evaluate the covariance function for an inla.spde
#'     object. Calculates the posterior distribution of the range,
#'     log(range), variance, or log(variance) parameter of a model's
#'     SPDE component. Can also calculate Matern correlation or covariance
#'     function.
#'
#' @keywords internal
#'
#' @param manifold Either "R1", "S1", "R2", or "S2", from
#'     `fm_manifold(mesh)`, or a full `fm_mesh_2d` or
#'     `fm_mesh_1d` object.
#' @param dist A vector of distances at which to calculate the
#'     covariances/correlations
#' @param log.range A scalar or a list (mean, sd), such as produced by
#'     `inla.spde.result(...)$summary.log.range.nominal[[1]][c("mean","sd")]`
#' @param log.variance Either `NULL`, a scalar, or vector of the
#'     same type as for log.range. When `NULL`, the correlations
#'     are calculated instead of the covariances.
#' @param alpha The SPDE operator order. Default 2.
#' @param quantile The target credible probability. Default 0.95.
#' @param n The number of parameter combinations to use for the
#'     approximation. Default 64.
#' @param S1.L For `manifold` `"S1"`, give the length of the
#'     cyclic interval
#' @return A list with estimated covariance or correlation (when `log.variance`
#'   is `NULL`) functions:
#' \item{lower}{An approximate lower bound for the `quantile` credible region}
#' \item{median}{The function for for the approximate median parameters
#' quantile}
#' \item{upper}{An approximate upper bound for the `quantile` credible region}
#'
#' @details
#' Uses a Gaussian assumption for the internal model parameters, and finds a
#' region in parameter space with approximately `quantile` probability.
#'
#' @author Finn Lindgren \email{Finn.Lindgren@@ed.ac.uk}
#' @export
materncov.bands <- function(manifold, dist, log.range,
                            log.variance = NULL, alpha = 2,
                            quantile = 0.95, n = 64, S1.L = NULL) {
  stopifnot(bru_safe_inla(multicore = TRUE))
  calc.cov.R <- function(dist, kappa, var) {
    INLA::inla.matern.cov(nu = nu, kappa, x = dist, d = d, corr = TRUE) * var
  }
  ## Do the right thing for _nominal_ variance
  calc.cov.S1 <- function(dist, kappa, var) {
    dist <- dist %% S1.L
    out <- calc.cov.R(abs(dist), kappa, var) + calc.cov.R(
      abs(dist - S1.L),
      kappa,
      var
    )
    # Measure the relative contribution of each term:
    cov0 <- calc.cov.R(0, kappa, 1)
    loop <- 0
    max.loop <- 10
    while ((cov0 > 1e-3) && (loop < max.loop)) {
      loop <- loop + 1
      out <- out + calc.cov.R(abs(dist + S1.L * loop), kappa, var) +
        calc.cov.R(abs(dist - S1.L - S1.L * loop), kappa, var)
      cov0 <- calc.cov.R(S1.L * loop, kappa, 1)
    }
    out
  }
  ## Do the right thing for _nominal_ variance
  calc.cov.S2 <- function(dist, kappa, var) {
    INLA::inla.matern.cov.s2(nu = nu, kappa, x = dist, norm.corr = FALSE) /
      INLA::inla.matern.cov(nu = nu, kappa, x = 0, d = 2, corr = FALSE) * var
  }
  calc.corr.R <- function(dist, kappa) {
    INLA::inla.matern.cov(nu = nu, kappa, x = dist, d = d, corr = TRUE)
  }
  calc.corr.S1 <- function(dist, kappa) {
    stop("Ooops, somehow inla.matern.cov.s1() is not available.")
    # INLA::inla.matern.cov.s1(nu=nu, kappa, x=dist, norm.corr=FALSE) /
    #   INLA::inla.matern.cov.s1(nu=nu, kappa, x=0, norm.corr=FALSE)
  }
  calc.corr.S2 <- function(dist, kappa) {
    INLA::inla.matern.cov.s2(nu = nu, kappa, x = dist, norm.corr = FALSE) /
      INLA::inla.matern.cov.s2(nu = nu, kappa, x = 0, norm.corr = FALSE)
  }
  if (!is.character(manifold)) {
    if (inherits(
      manifold,
      c("fm_mesh_2d", "inla.mesh", "fm_mesh_1d", "inla.mesh.1d")
    )) {
      if (fm_manifold(manifold, "S1") && is.null(S1.L)) {
        S1.L <- diff(manifold$interval)
      }
      manifold <- fm_manifold(manifold)
    }
  }
  if (manifold == "R1") {
    d <- 1
    calc.corr <- calc.corr.R
    calc.cov <- calc.cov.R
  } else if (manifold == "S1") {
    if (is.null(S1.L)) {
      stop(paste0("Manifold type 'S1' requires an interval length, 'S1.L'"))
    }
    d <- 1
    calc.corr <- calc.corr.S1
    calc.cov <- calc.cov.S1
  } else if (manifold == "R2") {
    d <- 2
    calc.corr <- calc.corr.R
    calc.cov <- calc.cov.R
  } else if (manifold == "S2") {
    d <- 2
    calc.corr <- calc.corr.S2
    calc.cov <- calc.cov.S2
  } else {
    stop(paste0(
      "materncov.bands does not support mesh manifold type ",
      manifold
    ))
  }
  nu <- alpha - d / 2
  if (!is.list(log.range)) {
    log.range <- list(mean = log.range, sd = 0)
  }
  probs <- c((1 - quantile) / 2, 0.5, (1 + quantile) / 2)
  if (is.null(log.variance)) {
    qq <- qnorm(probs)
    kappas <- sqrt(8 * nu) / exp(log.range$mean + log.range$sd * rev(qq))
    out <- data.frame(
      lower = calc.corr(dist, kappas[1]),
      median = calc.corr(dist, kappas[2]),
      upper = calc.corr(dist, kappas[3])
    )
  } else { ## !is.null(log.variance)
    if (!is.list(log.variance)) {
      log.variance <- list(mean = log.variance, sd = 0)
    }
    kappa <- sqrt(8 * nu) / exp(log.range$mean)
    out <- data.frame(
      lower = Inf,
      median = calc.cov(dist, kappa, exp(log.variance$mean)),
      upper = -Inf
    )
    if (log.variance$sd == 0) {
      out$lower <- out$median
      out$upper <- out$median
    } else {
      qq <- qchisq(quantile, 2)^0.5
      log.kappas <- log(sqrt(8 * nu)) - (log.range$mean +
        log.range$sd * c(qq, -qq))
      log.variances <- log.variance$mean + log.variance$sd * c(-qq, qq)
      for (angle in 2 * pi * (1:n) / n) {
        ca <- cos(angle)
        sa <- sin(angle)
        the.corr <- calc.cov(
          dist,
          exp(log.kappas[1] * (1 - ca) / 2 +
            log.kappas[2] * (ca + 1) / 2),
          exp(log.variances[1] * (1 - sa) / 2 +
            log.variances[2] * (sa + 1) / 2)
        )
        out$lower <- pmin(out$lower, the.corr)
        out$upper <- pmax(out$upper, the.corr)
      }
    }
  }
  out
}



#' @title Posteriors of SPDE hyper parameters and Matern correlation or
#'   covariance function.
#'
#' @description
#' Calculate posterior distribution of the range, log(range), variance, or
#' log(variance) parameter of a model's SPDE component. Can also plot Matern
#' correlation or covariance function. `inla.spde.result`.
#'
#' @param result An object inheriting from `inla`.
#' @param name Character stating the name of the SPDE effect, see
#'   `names(result$summary.random)`.
#' @param what One of "range", "log.range", "variance", "log.variance",
#'   "matern.correlation" or "matern.covariance".
#' @inheritParams materncov.bands
#' @return A `prediction` object.
#'
#' @export
#' @example inst/examples/spde.posterior.R
#'
#' @author Finn Lindgren \email{Finn.Lindgren@@ed.ac.uk}

spde.posterior <- function(result, name, what = "range",
                           quantile = 0.95) {
  stopifnot(bru_safe_inla(multicore = TRUE))

  spdespec <- result$bru_info$model$effects[[name]]$main$model
  spderesult <- INLA::inla.spde.result(result, name, spdespec)

  if (what == "matern.correlation" || what == "matern.covariance") {
    xmax <- exp(spderesult$summary.log.range.nominal[["0.975quant"]]) * 1.2
    x <- seq(0, xmax, length.out = 200)
    log.range <- list(
      mean = spderesult$summary.log.range.nominal[["mean"]],
      sd = spderesult$summary.log.range.nominal[["sd"]]
    )
    log.variance <- list(
      mean = spderesult$summary.log.variance.nominal[["mean"]],
      sd = spderesult$summary.log.variance.nominal[["sd"]]
    )
    if (what == "matern.correlation") {
      corr <- TRUE
      ylab <- "Matern Correlation"
      out <- materncov.bands(
        result$bru_info$model$effects[[name]]$main$mapper$mesh,
        dist = x,
        log.range = log.range,
        log.variance = NULL,
        alpha = 2, quantile = quantile
      )
    } else {
      corr <- FALSE
      ylab <- "Matern Covariance"
      out <- materncov.bands(
        result$bru_info$model$effects[[name]]$main$mapper$mesh,
        dist = x,
        log.range = log.range,
        log.variance = log.variance,
        alpha = 2, quantile = quantile
      )
    }


    df <- data.frame(
      x = x, q0.5 = out$median, lower = out$lower, upper = out$upper,
      median = out$median
    )
    colnames(df)[3] <- paste0("q", paste0(round((1 - quantile) / 2, 5), "%"))
    colnames(df)[4] <-
      paste0("q", paste0(round(1 - (1 - quantile) / 2, 5), "%"))
    attr(df, "type") <- "1d"
    attr(df, "misc") <- list(dims = "x", predictor = c("distance", ylab))
    class(df) <- list("prediction", "data.frame")
    df
  } else {
    marg <- switch(what,
      range = spderesult$marginals.range.nominal[[1]],
      log.range = spderesult$marginals.log.range.nominal[[1]],
      variance = spderesult$marginals.variance.nominal[[1]],
      log.variance = spderesult$marginals.log.variance.nominal[[1]]
    )
    if (is.null(marg)) {
      stop("Invalid varname: ", what, ". must be one of 'range',
                           'log.range',  'variance',  'log.variance',
                           'matern.correlation', matern.covariance")
    }

    med <- INLA::inla.qmarginal(0.5, marg)
    uq <- INLA::inla.qmarginal(1 - (1 - quantile) / 2, marg)
    lq <- INLA::inla.qmarginal((1 - quantile) / 2, marg)
    inner.x <- seq(lq, uq, length.out = 100)
    inner.marg <- data.frame(
      x = inner.x,
      y = INLA::inla.dmarginal(inner.x, marg)
    )
    colnames(inner.marg) <- c(what, "pdf")
    df <- data.frame(marg)
    colnames(df) <- c(what, "pdf")
    attr(df, "type") <- "0d"
    attr(df, "summary") <- list(
      uq = uq,
      lq = lq,
      median = med,
      inner.marg = inner.marg
    )
    class(df) <- list("prediction", "data.frame")
    df
  }
}
