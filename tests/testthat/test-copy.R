test_that("bru: inla copy feature", {
  skip_on_cran()
  local_bru_safe_inla()

  # Seed influences data as well as predict()!
  withr::local_seed(123L)

  df1 <- data.frame(x = cos(1:100))
  df2 <- data.frame(x = sin(1:100))
  df1 <- within(df1, y <- 1 + exp(2 * x) + rnorm(length(x), mean = 0, sd = 0.1))
  df2 <- within(df2, y <- 1 + (6 * x) + rnorm(length(x), mean = 0, sd = 0.1))

  cmp <- ~
    +1 +
      myLin1(x,
        model = "rw1",
        mapper = bru_mapper(fm_mesh_1d(seq(-1, 1, length.out = 100)),
          indexed = FALSE
        )
      ) +
      myLin2(x, copy = "myLin1", fixed = FALSE)
  cmps <- bru_comp_list(cmp)

  fit <- bru(
    cmp,
    bru_obs(
      y ~ Intercept + exp(myLin1),
      family = "gaussian",
      data = df1
    ),
    bru_obs(
      y ~ Intercept + (myLin2),
      family = "gaussian",
      data = df2
    ),
    options = list(control.inla = list(int.strategy = "eb"))
  )


  expect_equal(
    fit$summary.fixed["Intercept", "mean"],
    1,
    tolerance = midtol
  )
  expect_equal(
    fit$summary.hyperpar["Beta for myLin2", "mean"],
    3,
    tolerance = midtol
  )

  skip_if_not_installed("sn")
  pr <- predict(
    fit,
    data.frame(x = c(0.5, 1)),
    ~myLin2,
    n.samples = 500,
    seed = 1L
  )

  expect_equal(pr[, "mean"], c(3, 6), tolerance = midtol)
})


# test_that("bru: Non-copy option", {
#   skip_on_cran()
#   local_bru_safe_inla()
#
#   # Seed influences data as well as predict()!
#   withr::local_seed(123L)
#
#   df1 <- data.frame(x = cos(1:100))
#   df2 <- data.frame(x = sin(1:100))
#   df1 <- within(df1, eta <- 1 + 2 * x)
#   df2 <- within(df2, eta <- 1 + 6 * x)
#   df1 <- within(df1, y <- rnorm(length(eta), mean = eta, sd = 0.1))
#   df2 <- within(df2, y <- rpois(length(x), lambda = exp(eta)))
#
#   cmp <- ~
#     0 + I1(1) + I2(1) +
#     myLin1(x,
#            model = "rw1",
#            mapper = bru_mapper(fm_mesh_1d(seq(-1, 1, length.out = 100)),
#                                indexed = FALSE
#            ),
#            scale.model = TRUE
#     ) +
#         myLin2(x, copy = "myLin1", fixed = FALSE, initial = 1)
# #    myLin2(1)
#   cmps <- bru_comp_list(cmp)
#
#   fit <- bru(
#     cmp,
#     bru_obs(y ~ I1 + myLin1, family = "gaussian", data = df1),
# #    bru_obs(y ~ I2 + myLin2*myLin1, family = "poisson", data = df2),
#     bru_obs(y ~ I2 + myLin2, family = "poisson", data = df2),
#     options = list(control.inla = list(int.strategy = "eb"),
#                    bru_initial = list(myLin2 = 1, myLin1 = rnorm(100)))
#   )
#
#
#   expect_equal(
#     fit$summary.fixed["Intercept", "mean"],
#     1,
#     tolerance = midtol
#   )
#   expect_equal(
#     fit$summary.hyperpar["Beta for myLin2", "mean"],
#     3,
#     tolerance = midtol
#   )
#
#   skip_if_not_installed("sn")
#   pr <- predict(
#     fit,
#     data.frame(x = c(0.5, 1)),
#     ~myLin2,
#     n.samples = 500,
#     seed = 1L
#   )
#
#   expect_equal(pr[, "mean"], c(3, 6), tolerance = midtol)
# })


test_that("Component copy feature", {
  skip_on_cran()
  local_bru_safe_inla()

  # Seed influences data as well as predict()!
  withr::local_seed(123L)

  mydata <- data.frame(
    x0 = 1,
    x1 = rep(1:20, times = 2) / 10,
    x2 = rep(1:10, each = 4) / 10
  )
  mydata <- within(mydata, {
    y <- rpois(
      nrow(mydata),
      exp(x1^0.5 + x2^0.5 * 2 + x0 * 3)
    )
  })

  inlaform <- y ~ 0 + x0 +
    f(x1, model = "rw2", values = sort(unique(mydata$x1)), scale.model = TRUE) +
    f(x2, copy = "x1", fixed = FALSE)
  fit <- INLA::inla(
    formula = inlaform,
    data = mydata, family = "poisson",
    inla.mode = bru_options_get("inla.mode"),
    control.compute = list(config = TRUE),
    control.inla = list(int.strategy = "eb")
  )

  cmp <- y ~ 0 + x0 +
    x1(x1, model = "rw2", scale.model = TRUE) +
    x2(x2, copy = "x1", fixed = FALSE)
  fit_bru <- bru(
    cmp,
    family = "poisson",
    data = mydata,
    options = list(control.inla = list(int.strategy = "eb"))
  )

  expect_equal(
    fit_bru$summary.hyperpar$x1$mean,
    fit$summary.hyperpar$x1$mean,
    tolerance = lowtol
  )
  expect_equal(
    fit_bru$summary.hyperpar$x2$mean,
    fit$summary.hyperpar$x2$mean,
    tolerance = lowtol
  )
  expect_equal(
    fit_bru$summary.hyperpar$x1$sd,
    fit$summary.hyperpar$x1$sd,
    tolerance = hitol
  )
  expect_equal(
    fit_bru$summary.hyperpar$x2$sd,
    fit$summary.hyperpar$x2$sd,
    tolerance = hitol
  )
})

test_that("Component copy feature with group", {
  skip_on_cran()
  local_bru_safe_inla()

  # Seed influences data as well as predict()!
  withr::local_seed(123L)

  n <- c(16, 8)
  mydata <- data.frame(
    x1 = rep(seq_len(n[1]), times = n[2]),
    x2 = rep(seq_len(n[2]), each = n[1])
  )
  mydata <- rbind(mydata, mydata)
  mydata <- within(mydata, {
    Intercept <- 1
    z <- rep(c(1, 2), times = length(x1) / 2)
    z2 <- rep(c(1, 2), each = length(x1) / 2)
  })
  mydata <- within(mydata, {
    y <- rnorm(prod(n), x1^1.0 / n[1] * 4 + x2^1.0 / n[1] * 4 * 10, 1)
  })

  inlaform <- y ~ -1 + Intercept +
    f(
      x1,
      model = "rw1",
      values = seq_len(n[1]),
      scale.model = TRUE,
      group = z
    ) +
    f(x2,
      copy = "x1",
      fixed = FALSE,
      group = z2
    )
  fit <- INLA::inla(
    formula = inlaform, data = mydata, family = "normal",
    control.inla = list(int.strategy = "eb"),
    control.compute = list(config = TRUE),
    inla.mode = bru_options_get("inla.mode")
  )

  cmp <- ~ -1 + Intercept +
    x1(x1, model = "rw1", scale.model = TRUE, group = z) +
    x2(x2, copy = "x1", fixed = FALSE, group = z2)
  fit_bru <- bru(
    cmp,
    formula = y ~ Intercept + x1 + x2,
    family = "normal", data = mydata,
    options = list(
      bru_max_iter = 1,
      control.inla = list(int.strategy = "eb")
    )
  )

  # cmp2 <- ~ -1 + Intercept +
  #   x1(x1, model = "rw1", scale.model = TRUE, group = z) +
  #   x1copy(x2, copy = "x1", fixed = TRUE, group = z2) +
  #   beta(1, model = "linear")
  # fit_bru2 <- bru(
  #   cmp2,
  #   formula = y ~ Intercept + x1 + beta * exp(x1copy),
  #   family = "normal",
  #   data = mydata,
  #   options = list(
  #     bru_max_iter = 5,
  #     bru_initial = list(beta = 1),
  #     control.inla = list(int.strategy = "eb")
  #   )
  # )

  expect_equal(
    fit_bru$summary.hyperpar$mean,
    fit$summary.hyperpar$mean,
    tolerance = hitol
  )
  expect_equal(
    fit_bru$summary.hyperpar$sd,
    fit$summary.hyperpar$sd,
    tolerance = hitol
  )
})
