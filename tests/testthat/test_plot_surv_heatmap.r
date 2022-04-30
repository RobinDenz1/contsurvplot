
suppressMessages(requireNamespace("survival"))

sim_dat <- readRDS(system.file("testdata", "sim150.Rds",
                               package="contsurvplot"))

model <- survival::coxph(survival::Surv(time, event) ~ x3, data=sim_dat, x=TRUE)

test_that("plot, defaults", {
  plt <- plot_surv_heatmap(time="time", status="event", variable="x3",
                           data=sim_dat, model=model)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, defaults", fig=plt)
})

test_that("plot, cif", {
  plt <- plot_surv_heatmap(time="time", status="event", variable="x3",
                           data=sim_dat, model=model, cif=TRUE)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, cif", fig=plt)
})

test_that("plot, change horizon", {
  plt <- plot_surv_heatmap(time="time", status="event", variable="x3",
                           data=sim_dat, model=model,  horizon=seq(30, 60, 1))
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, change horizon", fig=plt)
})

test_that("plot, at t", {
  plt <- plot_surv_heatmap(time="time", status="event", variable="x3",
                           data=sim_dat, model=model, fixed_t=seq(0, 30, 1))
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, at t", fig=plt)
})

test_that("plot, custom colors", {
  plt <- plot_surv_heatmap(time="time", status="event", variable="x3",
                           data=sim_dat, model=model, start_color="grey",
                           end_color="black")
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, custom colors", fig=plt)
})

test_that("plot, panel_border / axis_dist", {
  plt <- plot_surv_heatmap(time="time", status="event", variable="x3",
                           data=sim_dat, model=model, panel_border=TRUE,
                           axis_dist=0.1)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, panel_border / axis_dist", fig=plt)
})

test_that("plot, contour_lines", {
  plt <- plot_surv_heatmap(time="time", status="event", variable="x3",
                           data=sim_dat, model=model, contour_lines=TRUE,
                           contour_size=1, contour_linetype="solid",
                           na.action=na.omit)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, contour_lines", fig=plt)
})

test_that("plot, lots of stuff", {
  plt <- plot_surv_heatmap(time="time", status="event", variable="x3",
                           data=sim_dat, model=model, start_color="grey",
                           end_color="black", cif=TRUE, contour_lines=TRUE,
                           contour_size=1, contour_linetype="dotdash",
                           title="Title", subtitle="Subtitle",
                           xlab="x", ylab="y",
                           legend.title="legend", legend.position="bottom",
                           gg_theme=ggplot2::theme_classic())
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, lots of stuff", fig=plt)
})
