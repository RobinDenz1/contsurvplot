
suppressMessages(requireNamespace("survival"))

sim_dat <- readRDS(system.file("testdata", "sim150.Rds",
                               package="contsurvplot"))
sim_dat$group <- factor(sim_dat$group)

model <- survival::coxph(survival::Surv(time, event) ~ x3 + group,
                         data=sim_dat, x=TRUE)

test_that("defaults, 1 time, 1 cont", {
  out <- curve_cont(data=sim_dat, variable="x3", model=model,
                    horizon=10, times=15)
  expect_true(is.data.frame(out))
  expect_true(all(c(out$time==15, round(out$est, 3)==0.724, out$cont==10)))
})

test_that("defaults, 1 time, 1 cont, group", {
  out <- curve_cont(data=sim_dat, variable="x3", model=model,
                    group="group", horizon=10, times=15)
  expect_true(is.data.frame(out))
  expect_true(all(c(out$time==15, round(out$est, 3)==c(0.687, 0.778),
                    out$cont==10)))
})

test_that("defaults, multiple times, multiple cont", {
  out <- curve_cont(data=sim_dat, variable="x3", model=model,
                    horizon=c(10, 15, 20), times=c(15, 16, 17, 18),
                    na.action=na.omit)
  expect_true(is.data.frame(out))
  expect_true(nrow(out)==12)
})

test_that("defaults, 1 time, 1 cont, cif", {
  out <- curve_cont(data=sim_dat, variable="x3", model=model,
                    horizon=10, times=15, cif=TRUE)
  expect_true(is.data.frame(out))
  expect_true(all(c(out$time==15, round(out$est, 3)==0.276, out$cont==10)))
})

test_that("defaults, multicore processing", {
  out <- curve_cont(data=sim_dat, variable="x3", model=model,
                    horizon=c(10, 15, 20), times=c(15, 16, 17, 18),
                    n_cores=2)
  expect_true(is.data.frame(out))
  expect_true(nrow(out)==12)
})
