
suppressMessages(requireNamespace("survival"))

sim_dat <- readRDS(system.file("testdata", "sim150.Rds",
                               package="contsurvplot"))
sim_dat$group <- as.factor(sim_dat$group)

model <- survival::coxph(survival::Surv(time, event) ~ x3 + group,
                         data=sim_dat, x=TRUE)

model_glm <- glm(time ~ x3 + x1 + group, data=sim_dat)
model_pecRpart <- list(rpart=list(terms=time ~ x3 + x1 + group))
class(model_pecRpart) <- "pecRpart"
model_ranger <- list(call=list(A="something", B=time ~ x3 + x1 + group))
class(model_ranger) <- "ranger"

test_that("glm model", {
  dat_clean <- prepare_inputdata(data=sim_dat,
                                 time="time",
                                 status="event",
                                 variable="x3",
                                 model=model_glm,
                                 na.action="na.omit")
  expect_true(nrow(sim_dat)==nrow(dat_clean))
  expect_true(ncol(dat_clean)==5)
})

test_that("pecRpart model", {
  dat_clean <- prepare_inputdata(data=sim_dat,
                                 time="time",
                                 status="event",
                                 variable="x3",
                                 model=model_pecRpart,
                                 na.action="na.omit")
  expect_true(nrow(sim_dat)==nrow(dat_clean))
  expect_true(ncol(dat_clean)==5)
})

test_that("ranger model", {
  dat_clean <- prepare_inputdata(data=sim_dat,
                                 time="time",
                                 status="event",
                                 variable="x3",
                                 model=model_ranger,
                                 na.action="na.omit")
  expect_true(nrow(sim_dat)==nrow(dat_clean))
  expect_true(ncol(dat_clean)==5)
})

test_that("NULL model", {
  dat_clean <- prepare_inputdata(data=sim_dat,
                                 time="time",
                                 status="event",
                                 variable="x3",
                                 model="A",
                                 na.action="na.omit")
  expect_true(nrow(sim_dat)==nrow(dat_clean))
  expect_true(ncol(dat_clean)==3)
})

test_that("wrong data", {
  expect_error(prepare_inputdata(data="wrong",
                                 time="time",
                                 status="event",
                                 variable="x3",
                                 model=model,
                                 na.action="na.omit"),
               "'data' must be a data.frame object.")
})

sim_dat$group <- NA

test_that("no data left after na.action", {
  expect_error(prepare_inputdata(data=sim_dat,
                                 time="time",
                                 status="event",
                                 variable="x3",
                                 model=model,
                                 na.action="na.omit"),
               "There is no data left after removing the missing values.")
})


