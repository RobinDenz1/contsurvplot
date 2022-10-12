
suppressMessages(requireNamespace("survival"))

sim_dat <- readRDS(system.file("testdata", "sim150.Rds",
                               package="contsurvplot"))
sim_dat$group <- factor(sim_dat$group)

model <- survival::coxph(survival::Surv(time, event) ~ x3 + group,
                         data=sim_dat, x=TRUE)

test_that("plot, defaults", {
  plt <- plot_surv_lines(time="time", status="event", variable="x3",
                         data=sim_dat, model=model)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, defaults", fig=plt)
})

test_that("plot, kaplan_meier", {
  plt <- plot_surv_lines(time="time", status="event", variable="x3",
                         data=sim_dat, model=model, kaplan_meier=TRUE,
                         km_color="black", km_size=1, km_linetype="dashed",
                         km_alpha=0.8, km_ci=TRUE, km_ci_type="log",
                         km_ci_level=0.9, km_ci_alpha=0.4)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, kaplan_meier", fig=plt)
})

test_that("plot, with ci", {
  set.seed(455)
  plt <- plot_surv_lines(time="time", status="event", variable="x3",
                         data=sim_dat, model=model, conf_int=TRUE,
                         n_boot=3, horizon=c(10, 30))
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, with ci", fig=plt)
})

test_that("plot, with group", {
  plt <- plot_surv_lines(time="time", status="event", variable="x3",
                         group="group", data=sim_dat, model=model)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, with group", fig=plt)
})

test_that("plot, cif", {
  plt <- plot_surv_lines(time="time", status="event", variable="x3",
                         data=sim_dat, model=model, cif=TRUE)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, cif", fig=plt)
})

test_that("plot, change horizon", {
  plt <- plot_surv_lines(time="time", status="event", variable="x3",
                         data=sim_dat, model=model,  horizon=seq(30, 60, 1))
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, change horizon", fig=plt)
})

test_that("plot, at t", {
  plt <- plot_surv_lines(time="time", status="event", variable="x3",
                         data=sim_dat, model=model, fixed_t=seq(0, 30, 1))
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, at t", fig=plt)
})

test_that("plot, custom colors", {
  plt <- plot_surv_lines(time="time", status="event", variable="x3",
                        data=sim_dat, model=model, horizon=c(7, 20, 45),
                        custom_colors=c("black", "red", "green"),
                        na.action=na.omit)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, custom colors", fig=plt)
})

test_that("plot, cont color", {
  plt <- plot_surv_lines(time="time", status="event", variable="x3",
                         data=sim_dat, model=model, discrete=FALSE,
                         start_color="red", end_color="green")
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, cont color", fig=plt)
})

test_that("plot, lots of stuff", {
  plt <- plot_surv_lines(time="time", status="event", variable="x3",
                         data=sim_dat, model=model, cif=TRUE, discrete=TRUE,
                         linetype="dashed", alpha=0.8, title="Title",
                         subtitle="Subtitle", xlab="x", ylab="y",
                         gg_theme=ggplot2::theme_classic())
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, lots of stuff", fig=plt)
})
