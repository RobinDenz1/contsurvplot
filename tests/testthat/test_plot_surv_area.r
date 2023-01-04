
suppressMessages(requireNamespace("survival"))

sim_dat <- readRDS(system.file("testdata", "sim150.Rds",
                               package="contsurvplot"))
sim_dat$group <- factor(sim_dat$group)

model <- survival::coxph(survival::Surv(time, event) ~ x3 + group,
                         data=sim_dat, x=TRUE)

sim_dat$x4 <- sim_dat$x3
sim_dat$x4[sim_dat$x4 > 30] <- sim_dat$x4[sim_dat$x4 > 30] - 100
model2 <- survival::coxph(survival::Surv(time, event) ~ splines::bs(x4, df=3),
                          data=sim_dat, x=TRUE)

test_that("plot, defaults", {
  plt <- plot_surv_area(time="time", status="event", variable="x3",
                        data=sim_dat, model=model)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, defaults", fig=plt)
})

test_that("plot, with group", {
  plt <- plot_surv_area(time="time", status="event", variable="x3",
                        group="group", data=sim_dat, model=model)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, with group", fig=plt)
})

test_that("plot, with nonmonotonic", {
  plt <- plot_surv_area(time="time", status="event", variable="x4",
                        monotonic=FALSE, data=sim_dat, model=model2)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, with nonmonotonic", fig=plt)
})

test_that("plot, cif", {
  plt <- plot_surv_area(time="time", status="event", variable="x3",
                        data=sim_dat, model=model, cif=TRUE,
                        na.action=na.omit)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, cif", fig=plt)
})

test_that("plot, change horizon", {
  plt <- plot_surv_area(time="time", status="event", variable="x3",
                        data=sim_dat, model=model,  horizon=seq(30, 60, 1))
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, change horizon", fig=plt)
})

test_that("plot, at t", {
  plt <- plot_surv_area(time="time", status="event", variable="x3",
                        data=sim_dat, model=model, fixed_t=seq(0, 30, 1))
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, at t", fig=plt)
})

test_that("plot, custom colors", {
  plt <- plot_surv_area(time="time", status="event", variable="x3",
                        data=sim_dat, model=model, start_color="grey",
                        end_color="black")
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, custom colors", fig=plt)
})

test_that("plot, discrete + bins", {
  plt <- plot_surv_area(time="time", status="event", variable="x3",
                        data=sim_dat, model=model, discrete=TRUE, bins=4)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, discrete + bins", fig=plt)
})

test_that("plot, discrete round", {
  plt <- plot_surv_area(time="time", status="event", variable="x3",
                        data=sim_dat, model=model, discrete=TRUE, bins=4,
                        label_digits=3)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, discrete round", fig=plt)
})

test_that("plot, sep_lines", {
  plt <- plot_surv_area(time="time", status="event", variable="x3",
                        data=sim_dat, model=model, discrete=TRUE,
                        sep_lines=TRUE, sep_size=0.2, sep_color="green",
                        sep_linetype="dashed")
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, sep_lines", fig=plt)
})

test_that("plot, with kaplan_meier", {
  plt <- plot_surv_area(time="time", status="event", variable="x3",
                        data=sim_dat, model=model, discrete=TRUE,
                        kaplan_meier=TRUE, km_size=1, km_color="white",
                        km_alpha=0.8, km_linetype="dashed")
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, with kaplan_meier", fig=plt)
})

test_that("plot, lots of stuff", {
  plt <- plot_surv_area(time="time", status="event", variable="x3",
                        data=sim_dat, model=model, start_color="grey",
                        end_color="black", cif=TRUE, discrete=TRUE,
                        bins=4, sep_lines=TRUE, sep_size=0.2, title="Title",
                        subtitle="Subtitle", xlab="x", ylab="y",
                        legend.title="legend", legend.position="bottom",
                        gg_theme=ggplot2::theme_classic())
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, lots of stuff", fig=plt)
})

test_that("using group with monotonic=FALSE", {
  expect_error(plot_surv_area(time="time", status="event", variable="x3",
                              monotonic=FALSE, group="group",
                              data=sim_dat, model=model),
               "The 'group' argument cannot be used with monotonic=FALSE.")
})

test_that("using discrete with monotonic=FALSE", {
  expect_warning(plot_surv_area(time="time", status="event", variable="x3",
                                monotonic=FALSE, discrete=TRUE,
                                data=sim_dat, model=model),
               paste0("To obtain valid results when using monotonic=FALSE, the",
                      " 'horizon' should contain at least 40 distinct values."))
})
