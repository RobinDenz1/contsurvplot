
suppressMessages(requireNamespace("survival"))

sim_dat <- readRDS(system.file("testdata", "sim150.Rds",
                               package="contsurvplot"))

model <- survival::coxph(survival::Surv(time, event) ~ x3, data=sim_dat, x=TRUE)

test_that("plot, defaults", {
  plt <- plot_surv_3Dsurface(time="time", status="event", variable="x3",
                             data=sim_dat, model=model)
  expect_true(is.matrix(plt))
  vdiffr::expect_doppelganger("plot, defaults", fig=plt)
})

test_that("plot, cif", {
  plt <- plot_surv_3Dsurface(time="time", status="event", variable="x3",
                             data=sim_dat, model=model, cif=TRUE,
                             na.action=na.omit)
  expect_true(is.matrix(plt))
  vdiffr::expect_doppelganger("plot, cif", fig=plt)
})

test_that("plot, change horizon", {
  plt <- plot_surv_3Dsurface(time="time", status="event", variable="x3",
                             data=sim_dat, model=model,  horizon=seq(30, 60, 1))
  expect_true(is.matrix(plt))
  vdiffr::expect_doppelganger("plot, change horizon", fig=plt)
})

test_that("plot, at t", {
  plt <- plot_surv_3Dsurface(time="time", status="event", variable="x3",
                             data=sim_dat, model=model, fixed_t=seq(0, 30, 1))
  expect_true(is.matrix(plt))
  vdiffr::expect_doppelganger("plot, at t", fig=plt)
})

test_that("plot, custom non-interactive", {
  plt <- plot_surv_3Dsurface(time="time", status="event", variable="x3",
                             data=sim_dat, model=model, col="red",
                             ticktype="simple", phi=30, theta=200, shade=0.3)
  expect_true(is.matrix(plt))
  vdiffr::expect_doppelganger("plot, custom non-interactive", fig=plt)
})

test_that("plot, interactive", {
  plt <- plot_surv_3Dsurface(time="time", status="event", variable="x3",
                             data=sim_dat, model=model, color="red",
                             interactive=TRUE)
  expect_s3_class(plt, "plotly")
})

test_that("plot, interactive + args", {
  plt <- plot_surv_3Dsurface(time="time", status="event", variable="x3",
                             data=sim_dat, model=model, color="red",
                             interactive=TRUE, xlab="x", ylab="y", zlab="z",
                             cif=TRUE)
  expect_s3_class(plt, "plotly")
})
