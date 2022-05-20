
suppressMessages(requireNamespace("survival"))

sim_dat <- readRDS(system.file("testdata", "sim150.Rds",
                               package="contsurvplot"))
sim_dat$group <- as.factor(sim_dat$group)

model <- survival::coxph(survival::Surv(time, event) ~ x3, data=sim_dat, x=TRUE)
wrong_model <- survival::coxph(survival::Surv(time, event) ~ x1,
                               data=sim_dat, x=TRUE)
wrong_model$coefficients[1] <- NA

test_that("time format", {
  expect_error(check_inputs_plots(time=c("A", "B"),
                                  status="event",
                                  variable="x3",
                                  data=sim_dat,
                                  model=model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t=NULL,
                                  max_t=Inf,
                                  discrete=FALSE,
                                  panel_border=TRUE,
                                  t=0.5,
                                  tau=10),
               paste0("'time' must be a single character ",
                      "string specifying a variable in 'data'."))
})

test_that("status format", {
  expect_error(check_inputs_plots(time="time",
                                  status=c("A", "B"),
                                  variable="x3",
                                  data=sim_dat,
                                  model=model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t=NULL,
                                  max_t=Inf,
                                  discrete=FALSE,
                                  panel_border=TRUE,
                                  t=0.5,
                                  tau=10),
               paste0("'status' must be a single character ",
                      "string specifying a variable in 'data'."))
})

test_that("variable format", {
  expect_error(check_inputs_plots(time="time",
                                  status="event",
                                  variable=c("A", "B"),
                                  data=sim_dat,
                                  model=model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t=NULL,
                                  max_t=Inf,
                                  discrete=FALSE,
                                  panel_border=TRUE,
                                  t=0.5,
                                  tau=10),
               paste0("'variable' must be a single character ",
                      "string specifying a variable in 'data'."))
})

test_that("variable format factor", {
  expect_error(check_inputs_plots(time="time",
                                  status="event",
                                  variable="group",
                                  data=sim_dat,
                                  model=model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t=NULL,
                                  max_t=Inf,
                                  discrete=FALSE,
                                  panel_border=TRUE,
                                  t=0.5,
                                  tau=10),
               paste0("The column specified by the 'variable' argument",
                      " must be numeric."))
})

test_that("time not in data", {
  expect_error(check_inputs_plots(time="time2",
                                  status="event",
                                  variable="x3",
                                  data=sim_dat,
                                  model=model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t=NULL,
                                  max_t=Inf,
                                  discrete=FALSE,
                                  panel_border=TRUE,
                                  t=0.5,
                                  tau=10),
               "time2 is not a valid column name in 'data'.")
})

test_that("status not in data", {
  expect_error(check_inputs_plots(time="time",
                                  status="event2",
                                  variable="x3",
                                  data=sim_dat,
                                  model=model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t=NULL,
                                  max_t=Inf,
                                  discrete=FALSE,
                                  panel_border=TRUE,
                                  t=0.5,
                                  tau=10),
               "event2 is not a valid column name in 'data'.")
})

test_that("variable not in data", {
  expect_error(check_inputs_plots(time="time",
                                  status="event",
                                  variable="x32",
                                  data=sim_dat,
                                  model=model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t=NULL,
                                  max_t=Inf,
                                  discrete=FALSE,
                                  panel_border=TRUE,
                                  t=0.5,
                                  tau=10),
               "x32 is not a valid column name in 'data'.")
})

test_that("time wrong in data", {
  expect_error(check_inputs_plots(time="group",
                                  status="event",
                                  variable="x3",
                                  data=sim_dat,
                                  model=model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t=NULL,
                                  max_t=Inf,
                                  discrete=FALSE,
                                  panel_border=TRUE,
                                  t=0.5,
                                  tau=10),
               "The column specified by the 'time' argument must be numeric.")
})

test_that("status wrong in data", {
  expect_error(check_inputs_plots(time="time",
                                  status="group",
                                  variable="x3",
                                  data=sim_dat,
                                  model=model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t=NULL,
                                  max_t=Inf,
                                  discrete=FALSE,
                                  panel_border=TRUE,
                                  t=0.5,
                                  tau=10),
               "The column specified by the 'status' argument must be numeric.")
})

test_that("variable not in model", {
  expect_error(check_inputs_plots(time="time",
                                  status="event",
                                  variable="x3",
                                  data=sim_dat,
                                  model=wrong_model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t=NULL,
                                  max_t=Inf,
                                  discrete=FALSE,
                                  panel_border=TRUE,
                                  t=0.5,
                                  tau=10),
               paste0("The 'variable' argument needs to be included as ",
                      "independent variable in the coxph model."))
})

test_that("NA in model coef", {
  expect_error(check_inputs_plots(time="time",
                                  status="event",
                                  variable="x1",
                                  data=sim_dat,
                                  model=wrong_model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t=NULL,
                                  max_t=Inf,
                                  discrete=FALSE,
                                  panel_border=TRUE,
                                  t=0.5,
                                  tau=10),
               paste0("Estimation impossible due to missing or infinite",
                      " coefficients in the coxph model."))
})

test_that("wrong fixed_t", {
  expect_error(check_inputs_plots(time="time",
                                  status="event",
                                  variable="x3",
                                  data=sim_dat,
                                  model=model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t="A",
                                  max_t=Inf,
                                  discrete=FALSE,
                                  panel_border=TRUE,
                                  t=0.5,
                                  tau=10),
               paste0("'fixed_t' must be a numeric vector with at least two",
                      " values or NULL."))
})

test_that("NA in fixed_t", {
  expect_error(check_inputs_plots(time="time",
                                  status="event",
                                  variable="x3",
                                  data=sim_dat,
                                  model=model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t=c(1, 2, NA),
                                  max_t=Inf,
                                  discrete=FALSE,
                                  panel_border=TRUE,
                                  t=0.5,
                                  tau=10),
               "Missing values in 'fixed_t' are not allowed.")
})

test_that("negative values in fixed_t", {
  expect_error(check_inputs_plots(time="time",
                                  status="event",
                                  variable="x3",
                                  data=sim_dat,
                                  model=model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t=c(-1, 2, 3),
                                  max_t=Inf,
                                  discrete=FALSE,
                                  panel_border=TRUE,
                                  t=0.5,
                                  tau=10),
          "Some values in 'fixed_t' are smaller than 0, which is not allowed.")
})

test_that("extrapolation in fixed_t", {
  expect_error(check_inputs_plots(time="time",
                                  status="event",
                                  variable="x3",
                                  data=sim_dat,
                                  model=model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t=c(1, 2, 10000),
                                  max_t=Inf,
                                  discrete=FALSE,
                                  panel_border=TRUE,
                                  t=0.5,
                                  tau=10),
              paste0("Some values in 'fixed_t' are bigger than the",
                     " maximum observed time in 'data', which is not allowed."))
})

test_that("wrong na.action", {
  expect_error(check_inputs_plots(time="time",
                                  status="event",
                                  variable="x3",
                                  data=sim_dat,
                                  model=model,
                                  na.action=1,
                                  horizon=NULL,
                                  fixed_t=NULL,
                                  max_t=Inf,
                                  discrete=FALSE,
                                  panel_border=TRUE,
                                  t=0.5,
                                  tau=10),
     "'na.action' must be a function or a single character string.")
})

test_that("wrong max_t", {
  expect_error(check_inputs_plots(time="time",
                                  status="event",
                                  variable="x3",
                                  data=sim_dat,
                                  model=model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t=NULL,
                                  max_t=c(1, 2),
                                  discrete=FALSE,
                                  panel_border=TRUE,
                                  t=0.5,
                                  tau=10),
               "'max_t' must be a single number.")
})

test_that("wrong discrete", {
  expect_error(check_inputs_plots(time="time",
                                  status="event",
                                  variable="x3",
                                  data=sim_dat,
                                  model=model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t=NULL,
                                  max_t=Inf,
                                  discrete=c(1, 2),
                                  panel_border=TRUE,
                                  t=0.5,
                                  tau=10),
               "'discrete' must be either TRUE or FALSE.")
})

test_that("wrong panel_border", {
  expect_error(check_inputs_plots(time="time",
                                  status="event",
                                  variable="x3",
                                  data=sim_dat,
                                  model=model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t=NULL,
                                  max_t=Inf,
                                  discrete=TRUE,
                                  panel_border=c(1, 2),
                                  t=0.5,
                                  tau=10),
               "'panel_border' must be either TRUE or FALSE.")
})

test_that("wrong t", {
  expect_error(check_inputs_plots(time="time",
                                  status="event",
                                  variable="x3",
                                  data=sim_dat,
                                  model=model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t=NULL,
                                  max_t=Inf,
                                  discrete=TRUE,
                                  panel_border=TRUE,
                                  t="0.5",
                                  tau=10),
               "'t' must be a numeric vector.")
})

test_that("wrong tau", {
  expect_error(check_inputs_plots(time="time",
                                  status="event",
                                  variable="x3",
                                  data=sim_dat,
                                  model=model,
                                  na.action="na.omit",
                                  horizon=NULL,
                                  fixed_t=NULL,
                                  max_t=Inf,
                                  discrete=TRUE,
                                  panel_border=TRUE,
                                  t=0.5,
                                  tau="10"),
               "'tau' must be a numeric vector.")
})
