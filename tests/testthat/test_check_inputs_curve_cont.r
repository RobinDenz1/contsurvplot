
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
                                       na.action="na.omit",
                                       contrast="none",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time=NULL,
                                       event_status=NULL),
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
                                       na.action="na.omit",
                                       contrast="none",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time=NULL,
                                       event_status=NULL),
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
                                       na.action="na.omit",
                                       contrast="none",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time=NULL,
                                       event_status=NULL),
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
                                       group=1,
                                       contrast="none",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time=NULL,
                                       event_status=NULL),
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
                                       na.action="na.omit",
                                       contrast="none",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time=NULL,
                                       event_status=NULL),
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
                                       na.action="na.omit",
                                       contrast="none",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time=NULL,
                                       event_status=NULL),
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
                                       na.action="na.omit",
                                       contrast="none",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time=NULL,
                                       event_status=NULL),
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
                                       na.action="na.omit",
                                       contrast="none",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time=NULL,
                                       event_status=NULL),
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
                                       na.action="na.omit",
                                       contrast="none",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time=NULL,
                                       event_status=NULL),
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
                                       na.action="na.omit",
                                       contrast="none",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time=NULL,
                                       event_status=NULL),
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
                                       na.action="na.omit",
                                       contrast="none",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time=NULL,
                                       event_status=NULL),
               paste0("The 'group' argument needs to be included as",
                      " independent variable in the 'model' object."))
})

test_that("wrong event_time", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x3",
                                       model=model,
                                       group=NULL,
                                       horizon=c(15, 16),
                                       times=c(1, 2, 3),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit",
                                       contrast="none",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time=1,
                                       event_status=NULL),
               paste0("'event_time' must be a single character string",
                      " specifying a variable in 'data'."))
})

test_that("event_time not in data", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x3",
                                       model=model,
                                       group=NULL,
                                       horizon=c(15, 16),
                                       times=c(1, 2, 3),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit",
                                       contrast="none",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time="x4",
                                       event_status=NULL),
               "x4 is not a valid column name in 'data'.")
})

test_that("event_time wrong type", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x3",
                                       model=model,
                                       group=NULL,
                                       horizon=c(15, 16),
                                       times=c(1, 2, 3),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit",
                                       contrast="none",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time="group",
                                       event_status=NULL),
        "The column specified by the 'event_time' argument must be numeric.")
})

test_that("wrong event_status", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x3",
                                       model=model,
                                       group=NULL,
                                       horizon=c(15, 16),
                                       times=c(1, 2, 3),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit",
                                       contrast="none",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time=NULL,
                                       event_status=1),
               paste0("'event_status' must be a single character string",
                      " specifying a variable in 'data'."))
})

test_that("event_status not in data", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x3",
                                       model=model,
                                       group=NULL,
                                       horizon=c(15, 16),
                                       times=c(1, 2, 3),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit",
                                       contrast="none",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time=NULL,
                                       event_status="x4"),
               "x4 is not a valid column name in 'data'.")
})

test_that("event_status wrong type", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x3",
                                       model=model,
                                       group=NULL,
                                       horizon=c(15, 16),
                                       times=c(1, 2, 3),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit",
                                       contrast="none",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time=NULL,
                                       event_status="group"),
        "The column specified by the 'event_status' argument must be numeric.")
})

test_that("correct contrast", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x3",
                                       model=model,
                                       group=NULL,
                                       horizon=c(15, 16),
                                       times=c(1, 2, 3),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit",
                                       contrast="NOONE",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time=NULL,
                                       event_status=NULL),
               "'contrast' must be one of c('none', 'diff', 'ratio').",
               fixed=TRUE)
})

test_that("correct reference", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x3",
                                       model=model,
                                       group=NULL,
                                       horizon=c(15, 16),
                                       times=c(1, 2, 3),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit",
                                       contrast="none",
                                       reference="asdfasdf",
                                       ref_value=NULL,
                                       event_time=NULL,
                                       event_status=NULL),
               "'reference' must be one of c('km', 'value').",
               fixed=TRUE)
})

test_that("correct ref_value", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x3",
                                       model=model,
                                       group=NULL,
                                       horizon=c(15, 16),
                                       times=c(1, 2, 3),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit",
                                       contrast="none",
                                       reference="km",
                                       ref_value="km",
                                       event_time=NULL,
                                       event_status=NULL),
               "'ref_value' must be a single number or NULL.",
               fixed=TRUE)
})

test_that("ref_value needs to be specified", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x3",
                                       model=model,
                                       group=NULL,
                                       horizon=c(15, 16),
                                       times=c(1, 2, 3),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit",
                                       contrast="diff",
                                       reference="value",
                                       ref_value=NULL,
                                       event_time=NULL,
                                       event_status=NULL),
              "'ref_value' needs to be specified when using reference='value'.",
               fixed=TRUE)
})

test_that("event_time, event_status needs to be specified", {
  expect_error(check_inputs_curve_cont(data=sim_dat,
                                       variable="x3",
                                       model=model,
                                       group=NULL,
                                       horizon=c(15, 16),
                                       times=c(1, 2, 3),
                                       cause=1,
                                       cif=FALSE,
                                       na.action="na.omit",
                                       contrast="diff",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time=NULL,
                                       event_status=NULL),
               paste0("Both 'event_time' and 'event_status' must be ",
                      "specified when using reference='km'."),
               fixed=TRUE)
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
                                       na.action="na.omit",
                                       contrast="none",
                                       reference="km",
                                       ref_value=NULL,
                                       event_time=NULL,
                                       event_status=NULL),
               paste0("Estimation impossible due to missing or infinite",
                      " coefficients in the coxph model."))
})
