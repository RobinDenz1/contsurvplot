
suppressMessages(requireNamespace("survival"))

sim_dat <- readRDS(system.file("testdata", "sim150.Rds",
                               package="contsurvplot"))

model <- survival::coxph(survival::Surv(time, event) ~ x3, data=sim_dat, x=TRUE)

test_that("plot, defaults", {
  plt <- plot_surv_rmst(time="time", status="event", variable="x3",
                        data=sim_dat, model=model, tau=35)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, defaults", fig=plt)
})

test_that("plot, change horizon", {
  plt <- plot_surv_rmst(time="time", status="event", variable="x3",
                        data=sim_dat, model=model,  horizon=seq(30, 60, 1),
                        tau=35, na.action=na.omit)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, change horizon", fig=plt)
})

test_that("plot, lots of stuff", {
  plt <- plot_surv_rmst(time="time", status="event", variable="x3",
                        data=sim_dat, model=model, tau=c(20, 37, 50),
                        linetype="dashed", alpha=0.8, title="Title",
                        subtitle="Subtitle", xlab="x", ylab="y",
                        gg_theme=ggplot2::theme_classic())
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, lots of stuff", fig=plt)
})
