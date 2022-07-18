
suppressMessages(requireNamespace("survival"))

sim_dat <- readRDS(system.file("testdata", "sim150.Rds",
                               package="contsurvplot"))
sim_dat$group <- as.factor(sim_dat$group)

model <- survival::coxph(survival::Surv(time, event) ~ x3 + group,
                         data=sim_dat, x=TRUE)
wrong_model <- survival::coxph(survival::Surv(time, event) ~ x1,
                               data=sim_dat, x=TRUE)

wrong_model_2 <- survival::coxph(survival::Surv(time, event) ~ x3,
                                 data=sim_dat, x=TRUE)

test_that("wrong variable", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable=1,
                                       model=model,
                                       group=NULL,
                                       horizon=c(15, 16),
                                       times=c(1, 2, 3),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit"),
               paste0("'variable' must be a single character string",
                      " specifying a variable in 'data'."))
})

test_that("variable not in data", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x4",
                                       model=model,
                                       group=NULL,
                                       horizon=c(15, 16),
                                       times=c(1, 2, 3),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit"),
               "x4 is not a valid column name in 'data'.")
})

test_that("variable wrong type", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="group",
                                       model=model,
                                       group=NULL,
                                       horizon=c(15, 16),
                                       times=c(1, 2, 3),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit"),
            "The column specified by the 'variable' argument must be numeric.")
})

test_that("wrong group", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x3",
                                       model=model,
                                       horizon=c(15, 16),
                                       times=c(1, 2, 3),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit",
                                       group=1),
               paste0("'group' must be a single character string specifying",
                      " a factor variable in 'data' or NULL."))
})

test_that("group not in data", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x3",
                                       model=model,
                                       group="x4",
                                       horizon=c(15, 16),
                                       times=c(1, 2, 3),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit"),
               paste0("x4 is not a valid column name in 'data'."))
})

test_that("group wrong type", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x3",
                                       model=model,
                                       group="x2",
                                       horizon=c(15, 16),
                                       times=c(1, 2, 3),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit"),
               paste0("The column specified by the 'group' argument",
                      " must be a factor."))
})

test_that("wrong times", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x3",
                                       model=model,
                                       group=NULL,
                                       horizon=c(15, 16),
                                       times="A",
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit"),
               "'times' must be a numeric vector with at least one value.")
})

test_that("NA in times", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x3",
                                       model=model,
                                       group=NULL,
                                       horizon=c(15, 16),
                                       times=c(15, 16, NA),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit"),
               "Missing values in 'times' are not allowed.")
})

test_that("times too small", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x3",
                                       model=model,
                                       group=NULL,
                                       horizon=c(15, 16),
                                       times=c(-1, 15, 16),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit"),
            "Some values in 'times' are smaller than 0, which is not allowed.")
})

test_that("variable not in cox model", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x3",
                                       model=wrong_model,
                                       group=NULL,
                                       horizon=c(15, 16),
                                       times=c(15, 16),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit"),
               paste0("The 'variable' argument needs to be included as",
                      " independent variable in the 'model' object."))
})

test_that("group not in cox model", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x3",
                                       model=wrong_model_2,
                                       group="group",
                                       horizon=c(15, 16),
                                       times=c(15, 16),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit"),
               paste0("The 'group' argument needs to be included as",
                      " independent variable in the 'model' object."))
})

wrong_model$coefficients[1] <- NA

test_that("NA in cox model coefficients", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x1",
                                       model=wrong_model,
                                       group=NULL,
                                       horizon=c(1, 2),
                                       times=c(15, 16),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit"),
               paste0("Estimation impossible due to missing or infinite",
                      " coefficients in the coxph model."))
})
