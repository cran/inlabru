test_that("2D LGCP fitting and prediction: Plot sampling", {
  skip_on_cran()
  local_bru_safe_inla()
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  options <- list(
    control.inla = list(
      int.strategy = "eb"
    )
  )

  gorillas <- gorillas_sf

  matern <- INLA::inla.spde2.pcmatern(gorillas$mesh,
    prior.sigma = c(0.1, 0.01),
    prior.range = c(5, 0.01)
  )

  #  if (FALSE) {
  #    bm <- bench::mark(
  #      ips_old = ipoints(
  #        samplers = gorillas$plotsample$plots,
  #        domain = gorillas$mesh,
  #        int.args = list(use_new = FALSE)
  #      ),
  #      ips_new = ipoints(
  #        samplers = gorillas$plotsample$plots,
  #        domain = gorillas$mesh,
  #        int.args = list(use_new = TRUE)
  #      ),
  #      check = FALSE)
  #    ips_old <- ipoints(
  #      samplers = gorillas$plotsample$plots,
  #      domain = gorillas$mesh,
  #      int.args = list(use_new = FALSE)
  #    )
  #    pl <- gorillas$plotsample$plots
  #    pl$ID <- seq_len(NROW(pl))
  #    ips_new <- ipoints(
  #      samplers = pl,
  #      domain = gorillas$mesh,
  #      int.args = list(use_new = TRUE),
  #      group = "ID"
  #    )
  #    ggplot() +
  #      gg(gorillas$mesh) +
  #      gg(ips_old, aes(size=weight, col="old")) +
  #      gg(ips_new, aes(size=weight, col = "new"))
  #    identical(ips_old, ips_new)
  #    ggplot() +
  #      gg(gorillas$mesh) +
  #      gg(ips_new, aes(size=weight, col = factor(ID)))
  #  }

  cmp <- geometry ~ my.spde(main = geometry, model = matern)
  fit <- lgcp(cmp,
    data = gorillas$plotsample$nests,
    samplers = gorillas$plotsample$plots,
    domain = list(geometry = gorillas$mesh),
    options = options
  )

  expect_equal(
    sum(fit$bru_info$lhoods[[1]]$E),
    7.092,
    tolerance = lowtol
  )
  # Test points selected with sd+sd less than 1.2, for a more stable check.
  mean_est <- fit$summary.fixed["Intercept", "mean"] +
    fit$summary.random$my.spde$mean[c(19, 100, 212)]
  expect_snapshot_value(
    mean_est,
    tolerance = midtol,
    style = "serialize"
  )
  sd_est <- fit$summary.fixed["Intercept", "sd"] +
    fit$summary.random$my.spde$sd[c(19, 100, 212)]
  expect_snapshot_value(
    sd_est,
    tolerance = hitol,
    style = "serialize"
  )
})
