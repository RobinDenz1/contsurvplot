
suppressMessages(requireNamespace("survival"))

sim_dat <- readRDS(system.file("testdata", "sim150.Rds",
                               package="contsurvplot"))
sim_dat$group <- factor(sim_dat$group)

model <- survival::coxph(survival::Surv(time, event) ~ x3 + group,
                         data=sim_dat, x=TRUE)

test_that("plot, defaults", {
  plt <- plot_surv_quantiles(time="time", status="event", variable="x3",
                             data=sim_dat, model=model)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, defaults", fig=plt)
})

test_that("plot, with group", {
  plt <- plot_surv_quantiles(time="time", status="event", variable="x3",
                             group="group", data=sim_dat, model=model)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, with group", fig=plt)
})

test_that("plot, change horizon", {
  plt <- plot_surv_quantiles(time="time", status="event", variable="x3",
                             data=sim_dat, model=model,  horizon=seq(30, 60, 1))
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, change horizon", fig=plt)
})

test_that("plot, custom colors", {
  plt <- plot_surv_quantiles(time="time", status="event", variable="x3",
                             data=sim_dat, model=model, p=c(0.25, 0.5, 0.75),
                             custom_colors=c("black", "red", "green"))
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, custom colors", fig=plt)
})

test_that("plot, single color", {
  plt <- plot_surv_quantiles(time="time", status="event", variable="x3",
                             data=sim_dat, model=model, p=c(0.25, 0.5, 0.75),
                             single_color="blue", na.action=na.omit)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, single colors", fig=plt)
})

test_that("plot, lots of stuff", {
  plt <- plot_surv_quantiles(time="time", status="event", variable="x3",
                             data=sim_dat, model=model,
                             linetype="dashed", alpha=0.8, title="Title",
                             subtitle="Subtitle", xlab="x", ylab="y",
                             gg_theme=ggplot2::theme_classic())
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, lots of stuff", fig=plt)
})
