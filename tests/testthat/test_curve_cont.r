
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

test_that("defaults, 1 time, 1 cont, boot", {
  set.seed(42)
  out <- curve_cont(data=sim_dat, variable="x3", model=model,
                    horizon=10, times=15, conf_int=TRUE, n_boot=2)
  expect_true(is.data.frame(out))
  expect_true(all(c(out$time==15, round(out$est, 3)==0.724, out$cont==10,
                    round(out$se, 3)==0.004, round(out$ci_lower, 3)==0.795,
                    round(out$ci_upper, 3)==0.8)))
})

test_that("defaults, 1 time, 1 cont, boot, multicore", {
  out <- curve_cont(data=sim_dat, variable="x3", model=model,
                    horizon=10, times=15, conf_int=TRUE, n_boot=2,
                    n_cores=2)
  expect_true(is.data.frame(out))
  expect_true(nrow(out)==1)
})

test_that("defaults, 1 time, 1 cont, group, boot", {
  set.seed(42)
  out <- curve_cont(data=sim_dat, variable="x3", model=model,
                    horizon=10, times=15, conf_int=TRUE, n_boot=2,
                    group="group")
  expect_true(is.data.frame(out))
  expect_true(all(c(out$time==15,
                    round(out$est, 3)==c(0.687, 0.778),
                    out$cont==10,
                    round(out$se, 3)==c(0.018, 0.020),
                    round(out$ci_lower, 3)==c(0.753, 0.830),
                    round(out$ci_upper, 3)==c(0.777, 0.858))))
})
