
suppressMessages(requireNamespace("survival"))

sim_dat <- readRDS(system.file("testdata", "sim150.Rds",
                               package="contsurvplot"))
sim_dat$group <- factor(sim_dat$group)

model <- survival::coxph(survival::Surv(time, event) ~ x3 + group,
                         data=sim_dat, x=TRUE)

test_that("plot, defaults", {
  plt <- plot_surv_matrix(time="time", status="event", variable="x3",
                          data=sim_dat, model=model)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, defaults", fig=plt)
})

test_that("plot, cif", {
  plt <- plot_surv_matrix(time="time", status="event", variable="x3",
                          data=sim_dat, model=model, cif=TRUE)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, cif", fig=plt)
})

test_that("plot, change horizon", {
  plt <- plot_surv_matrix(time="time", status="event", variable="x3",
                          data=sim_dat, model=model,  horizon=seq(30, 60, 1))
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, change horizon", fig=plt)
})

test_that("plot, change fixed_t", {
  plt <- plot_surv_matrix(time="time", status="event", variable="x3",
                          data=sim_dat, model=model, fixed_t=seq(0, 30, 1))
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, change fixed_t", fig=plt)
})

test_that("plot, custom colors", {
  plt <- plot_surv_matrix(time="time", status="event", variable="x3",
                          data=sim_dat, model=model, start_color="grey",
                          end_color="black")
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, custom colors", fig=plt)
})

test_that("plot, panel_border / axis_dist", {
  plt <- plot_surv_matrix(time="time", status="event", variable="x3",
                          data=sim_dat, model=model, panel_border=TRUE,
                          axis_dist=0.1)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, panel_border / axis_dist", fig=plt)
})

test_that("plot, change n_col and n_row", {
  plt <- plot_surv_matrix(time="time", status="event", variable="x3",
                          data=sim_dat, model=model, n_row=5, n_col=5)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, change n_col and n_row", fig=plt)
})

test_that("plot, change border stuff", {
  plt <- plot_surv_matrix(time="time", status="event", variable="x3",
                          data=sim_dat, model=model, border_color="black",
                          border_size=2)
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, change border stuff", fig=plt)
})

test_that("plot, change numbers stuff", {
  plt <- plot_surv_matrix(time="time", status="event", variable="x3",
                          data=sim_dat, model=model,
                          number_color="black", number_digits=1,
                          number_size=5, number_fontface="italic",
                          number_family="serif")
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, change numbers stuff", fig=plt)
})

test_that("plot, using group", {
  plt <- plot_surv_matrix(time="time", status="event", variable="x3",
                          data=sim_dat, model=model, group="group")
  expect_s3_class(plt, "ggplot")
  vdiffr::expect_doppelganger("plot, using group", fig=plt)
})

test_that("n_col too big", {
  expect_error(plot_surv_matrix(time="time", status="event", variable="x3",
                                data=sim_dat, model=model, n_col=105),
               paste0("'n_col' must be smaller than length(fixed_t). ",
                      "Decrease n_col or increase the number of points ",
                      "in time used in the estimation."), fixed=TRUE)
})

test_that("n_row too big", {
  expect_error(plot_surv_matrix(time="time", status="event", variable="x3",
                                data=sim_dat, model=model, n_row=105),
               paste0("'n_row' must be smaller than length(horizon). ",
                      "Decrease n_row or increase the number of values ",
                      "in horizon used in the estimation."), fixed=TRUE)
})
