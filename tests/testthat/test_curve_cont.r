
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

test_that("return_boot", {
  out <- curve_cont(data=sim_dat, variable="x3", model=model,
                    horizon=10, times=15, conf_int=TRUE, n_boot=2,
                    n_cores=1, return_boot=TRUE)
  expect_true(is.data.frame(out))
  expect_true(nrow(out)==2)
})

test_that("contrasts working", {
  out_10 <- curve_cont(data=sim_dat, variable="x3", model=model,
                       horizon=10, times=c(10, 15))
  out_15 <- curve_cont(data=sim_dat, variable="x3", model=model,
                       horizon=11, times=c(10, 15))
  out_km <- get_kaplan_meier(time="time", status="event", data=sim_dat,
                             fixed_t=c(10, 15))

  out_diff_value <- curve_cont(data=sim_dat, variable="x3", model=model,
                               horizon=10, times=c(10, 15),
                               contrast="diff", reference="value", ref_value=11)
  out_ratio_value <- curve_cont(data=sim_dat, variable="x3", model=model,
                                horizon=10, times=c(10, 15),
                                contrast="ratio", reference="value",
                                ref_value=11)

  out_diff_km <- curve_cont(data=sim_dat, variable="x3", model=model,
                            horizon=10, times=c(10, 15),
                            contrast="diff", reference="km",
                            event_time="time", event_status="event")
  out_ratio_km <- curve_cont(data=sim_dat, variable="x3", model=model,
                             horizon=10, times=c(10, 15),
                             contrast="ratio", reference="km",
                             event_time="time", event_status="event")

  # difference value
  expect_true(all(out_15$est - out_10$est==out_diff_value$est))

  # difference km
  expect_true(all(out_km$est - out_10$est==out_diff_km$est))

  # ratio value
  expect_true(all(out_15$est / out_10$est==out_ratio_value$est))

  # ratio km
  expect_true(all(out_km$est / out_10$est==out_ratio_km$est))
})

test_that("conf_int, diff, km", {
  set.seed(333)
  out <- curve_cont(data=sim_dat, variable="x3", model=model,
                    horizon=c(10, 15), times=15, conf_int=TRUE, n_boot=2,
                    n_cores=1, contrast="diff", reference="km",
                    event_time="time", event_status="event")
  expect_true(is.data.frame(out))
  expect_true(all(c(out$time==c(15, 15),
                    round(out$est, 3)==c(0.046, 0.035),
                    out$cont==c(10, 15),
                    round(out$se, 3)==c(0.015, 0.013),
                    round(out$ci_lower, 3)==c(0.070, 0.052),
                    round(out$ci_upper, 3)==c(0.090, 0.069))))
})

test_that("conf_int, ratio, km", {
  set.seed(333)
  out <- curve_cont(data=sim_dat, variable="x3", model=model,
                    horizon=c(10, 15), times=15, conf_int=TRUE, n_boot=2,
                    n_cores=1, contrast="ratio", reference="km",
                    event_time="time", event_status="event")
  expect_true(is.data.frame(out))
  expect_true(all(c(out$time==c(15, 15),
                    round(out$est, 3)==c(1.063, 1.048),
                    out$cont==c(10, 15),
                    round(out$se, 3)==c(0.033, 0.027),
                    round(out$ci_lower, 3)==c(1.093, 1.067),
                    round(out$ci_upper, 3)==c(1.137, 1.103))))
})

test_that("conf_int, diff, value", {
  set.seed(333)
  out <- curve_cont(data=sim_dat, variable="x3", model=model,
                    horizon=c(10, 15), times=15, conf_int=TRUE, n_boot=2,
                    n_cores=1, contrast="diff", reference="value",
                    ref_value=12)
  expect_true(is.data.frame(out))
  expect_true(all(c(out$time==c(15, 15),
                    round(out$est, 3)==c(0.004, -0.006),
                    out$cont==c(10, 15),
                    round(out$se, 3)==c(0.001, 0.001),
                    round(out$ci_lower, 3)==c(0.007, -0.012),
                    round(out$ci_upper, 3)==c(0.008, -0.011))))
})

test_that("conf_int, ratio, value", {
  set.seed(333)
  out <- curve_cont(data=sim_dat, variable="x3", model=model,
                    horizon=c(10, 15), times=15, conf_int=TRUE, n_boot=2,
                    n_cores=1, contrast="ratio", reference="value",
                    ref_value=12)
  expect_true(is.data.frame(out))
  expect_true(all(c(out$time==c(15, 15),
                    round(out$est, 3)==c(1.006, 0.991),
                    out$cont==c(10, 15),
                    round(out$se, 3)==c(0.002, 0.003),
                    round(out$ci_lower, 3)==c(1.010, 0.982),
                    round(out$ci_upper, 3)==c(1.013, 0.986))))
})

test_that("contrast, conf_int", {
  out <- curve_cont(data=sim_dat, variable="x3", model=model,
                    horizon=10, times=15, conf_int=TRUE, n_boot=2,
                    n_cores=1, contrast="diff", reference="km",
                    event_time="time", event_status="event",
                    group="group")
  expect_true(is.data.frame(out))
  expect_true(nrow(out)==2)
})
