
suppressMessages(requireNamespace("survival"))

sim_dat <- readRDS(system.file("testdata", "sim150.Rds",
                               package="contsurvplot"))
sim_dat$group <- factor(sim_dat$group)

model <- survival::coxph(survival::Surv(time, event) ~ x3 + group,
                         data=sim_dat, x=TRUE)

test_that("plot, defaults", {
  plt <- plot_surv_animated(time="time", status="event", variable="x3",
                            data=sim_dat, model=model)
  expect_s3_class(plt, "plotly")
})

test_that("plot, cif", {
  plt <- plot_surv_animated(time="time", status="event", variable="x3",
                            data=sim_dat, model=model, cif=TRUE)
  expect_s3_class(plt, "plotly")
})

test_that("plot, change horizon", {
  plt <- plot_surv_animated(time="time", status="event", variable="x3",
                            data=sim_dat, model=model,  horizon=seq(30, 60, 1))
  expect_s3_class(plt, "plotly")
})

test_that("plot, at t", {
  plt <- plot_surv_animated(time="time", status="event", variable="x3",
                            data=sim_dat, model=model, fixed_t=seq(0, 30, 1),
                            na.action=na.omit)
  expect_s3_class(plt, "plotly")
})

test_that("plot, custom color", {
  plt <- plot_surv_animated(time="time", status="event", variable="x3",
                            data=sim_dat, model=model, color="red")
  expect_s3_class(plt, "plotly")
})

test_that("plot, no slider", {
  plt <- plot_surv_animated(time="time", status="event", variable="x3",
                            data=sim_dat, model=model, color="red",
                            slider=FALSE)
  expect_s3_class(plt, "gganim")
})

test_that("plot, with group", {
  plt <- plot_surv_animated(time="time", status="event", variable="x3",
                            group="group", data=sim_dat, model=model)
  expect_s3_class(plt, "plotly")
})

test_that("plot, lots of stuff", {
  plt <- plot_surv_animated(time="time", status="event", variable="x3",
                            data=sim_dat, model=model, cif=TRUE,
                            linetype="dashed", alpha=0.8, title="Title",
                            subtitle="Subtitle", xlab="x", ylab="y",
                            gg_theme=ggplot2::theme_classic())
  expect_s3_class(plt, "plotly")
})
