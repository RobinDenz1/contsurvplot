
suppressMessages(requireNamespace("survival"))

sim_dat <- readRDS(system.file("testdata", "sim150.Rds",
                               package="contsurvplot"))

test_that("wrong type", {
  expect_error(check_horizon(horizon="a",
                             data=sim_dat,
                             variable="x2"),
        "'horizon' must be a numeric vector with at least one value or NULL.")
})

test_that("NA in horizon", {
  expect_error(check_horizon(horizon=c(1, 2, NA),
                             data=sim_dat,
                             variable="x2"),
               "Missing values in 'horizon' are not allowed.")
})

test_that("too small", {
  expect_error(check_horizon(horizon=c(-100, 2),
                             data=sim_dat,
                             variable="x2"),
               paste0("Some values in 'horizon' are smaller than the",
                      " minimum observed in 'data', which is not allowed."))
})

test_that("too small", {
  expect_error(check_horizon(horizon=c(100, 2),
                             data=sim_dat,
                             variable="x2"),
               paste0("Some values in 'horizon' are bigger than the",
                      " maximum observed in 'data', which is not allowed."))
})
