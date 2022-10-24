
suppressMessages(requireNamespace("survival"))

sim_dat <- readRDS(system.file("testdata", "sim150.Rds",
                               package="contsurvplot"))
sim_dat$group <- factor(sim_dat$group)

model <- survival::coxph(survival::Surv(time, event) ~ x3 + group,
                         data=sim_dat, x=TRUE)

test_that("standard case", {
  out <- get_kaplan_meier(time="time", status="event", data=sim_dat)
  expect_true(is.data.frame(out))
  expect_true(ncol(out)==2 & nrow(out)==151)
})

test_that("with conf_int", {
  out <- get_kaplan_meier(time="time", status="event", data=sim_dat,
                          conf_int=TRUE)
  expect_true(is.data.frame(out))
  expect_true(ncol(out)==4 & nrow(out)==151)
})

test_that("with group", {
  out <- get_kaplan_meier(time="time", status="event", data=sim_dat,
                          group="group")
  expect_true(is.data.frame(out))
  expect_true(ncol(out)==3 & nrow(out)==152)
})

test_that("with group and conf_int", {
  out <- get_kaplan_meier(time="time", status="event", data=sim_dat,
                          conf_int=TRUE, group="group")
  expect_true(is.data.frame(out))
  expect_true(ncol(out)==5 & nrow(out)==152)
})

test_that("with conf_int and cif", {
  out <- get_kaplan_meier(time="time", status="event", data=sim_dat,
                          conf_int=TRUE, cif=TRUE)
  expect_true(is.data.frame(out))
  expect_true(ncol(out)==4 & nrow(out)==151)
})

test_that("fixed_t", {
  out <- get_kaplan_meier(time="time", status="event", data=sim_dat,
                          fixed_t=c(22, 40))
  expect_true(is.data.frame(out))
  expect_true(ncol(out)==2 & nrow(out)==2 & all(out$time==c(22, 40)))
})

test_that("fixed_t, with group", {
  out <- get_kaplan_meier(time="time", status="event", data=sim_dat,
                          group="group", fixed_t=c(22, 40))
  expect_true(is.data.frame(out))
  expect_true(ncol(out)==3 & nrow(out)==4 & all(out$time==c(22, 40, 22, 40)))
})
