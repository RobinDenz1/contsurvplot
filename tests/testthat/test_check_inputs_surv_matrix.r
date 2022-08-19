
good_fixed_t <- seq(0, 100, 1)
bad_fixed_t <- c(good_fixed_t, 3, 8, 17)
good_horizon <- seq(0, 100, 1)
bad_horizon <- c(good_horizon, 3, 8, 17)

test_that("fixed_t format", {
  expect_error(check_inputs_surv_matrix(border_color="white",
                                        border_size=2,
                                        numbers=TRUE,
                                        number_color="white",
                                        number_size=3,
                                        number_family="sans",
                                        number_fontface="serif",
                                        number_digits=3,
                                        fixed_t=bad_fixed_t,
                                        horizon=good_horizon,
                                        n_col=10,
                                        n_row=10),
               paste0("'fixed_t' needs to be an equally spaced numeric ",
                      "vector. For example, you could use seq(min, ",
                      "max, length.out=100)."), fixed=TRUE)
})

test_that("horizon format", {
  expect_error(check_inputs_surv_matrix(border_color="white",
                                        border_size=2,
                                        numbers=TRUE,
                                        number_color="white",
                                        number_size=3,
                                        number_family="sans",
                                        number_fontface="serif",
                                        number_digits=3,
                                        fixed_t=good_fixed_t,
                                        horizon=bad_horizon,
                                        n_col=10,
                                        n_row=10),
               paste0("'horizon' needs to be an equally spaced numeric ",
                      "vector. For example, you could use seq(min, ",
                      "max, length.out=100)."), fixed=TRUE)
})

test_that("border_color format", {
  expect_error(check_inputs_surv_matrix(border_color=2,
                                        border_size=2,
                                        numbers=TRUE,
                                        number_color="white",
                                        number_size=3,
                                        number_family="sans",
                                        number_fontface="serif",
                                        number_digits=3,
                                        fixed_t=good_fixed_t,
                                        horizon=good_horizon,
                                        n_col=10,
                                        n_row=10),
               paste0("'border_color' needs to be a single character string ",
                      "specifying the color of the rectangle borders or NULL."))
})

test_that("border_size format", {
  expect_error(check_inputs_surv_matrix(border_color="white",
                                        border_size="A",
                                        numbers=TRUE,
                                        number_color="white",
                                        number_size=3,
                                        number_family="sans",
                                        number_fontface="serif",
                                        number_digits=3,
                                        fixed_t=good_fixed_t,
                                        horizon=good_horizon,
                                        n_col=10,
                                        n_row=10),
               "'border_size' needs to be a single number.")
})

test_that("numbers format", {
  expect_error(check_inputs_surv_matrix(border_color="white",
                                        border_size=2,
                                        numbers=1,
                                        number_color="white",
                                        number_size=3,
                                        number_family="sans",
                                        number_fontface="serif",
                                        number_digits=3,
                                        fixed_t=good_fixed_t,
                                        horizon=good_horizon,
                                        n_col=10,
                                        n_row=10),
               "'numbers' must be either TRUE or FALSE.")
})

test_that("number_color format", {
  expect_error(check_inputs_surv_matrix(border_color="white",
                                        border_size=2,
                                        numbers=TRUE,
                                        number_color=3,
                                        number_size=3,
                                        number_family="sans",
                                        number_fontface="serif",
                                        number_digits=3,
                                        fixed_t=good_fixed_t,
                                        horizon=good_horizon,
                                        n_col=10,
                                        n_row=10),
               paste0("'number_color' needs to be a single character string ",
                      "specifying the color of the text inside the ",
                      "rectangles."))
})

test_that("number_size format", {
  expect_error(check_inputs_surv_matrix(border_color="white",
                                        border_size=2,
                                        numbers=TRUE,
                                        number_color="white",
                                        number_size="3",
                                        number_family="sans",
                                        number_fontface="serif",
                                        number_digits=3,
                                        fixed_t=good_fixed_t,
                                        horizon=good_horizon,
                                        n_col=10,
                                        n_row=10),
               "'number_size' needs to be a single number.")
})

test_that("number_family format", {
  expect_error(check_inputs_surv_matrix(border_color="white",
                                        border_size=2,
                                        numbers=TRUE,
                                        number_color="white",
                                        number_size=3,
                                        number_family=3,
                                        number_fontface="serif",
                                        number_digits=3,
                                        fixed_t=good_fixed_t,
                                        horizon=good_horizon,
                                        n_col=10,
                                        n_row=10),
               "'number_family' needs to be a single character string.")
})

test_that("number_fontface format", {
  expect_error(check_inputs_surv_matrix(border_color="white",
                                        border_size=2,
                                        numbers=TRUE,
                                        number_color="white",
                                        number_size=3,
                                        number_family="sans",
                                        number_fontface=4,
                                        number_digits=3,
                                        fixed_t=good_fixed_t,
                                        horizon=good_horizon,
                                        n_col=10,
                                        n_row=10),
               "'number_fontface' needs to be a single character string.")
})

test_that("number_digits format", {
  expect_error(check_inputs_surv_matrix(border_color="white",
                                        border_size=2,
                                        numbers=TRUE,
                                        number_color="white",
                                        number_size=3,
                                        number_family="sans",
                                        number_fontface="serif",
                                        number_digits="A",
                                        fixed_t=good_fixed_t,
                                        horizon=good_horizon,
                                        n_col=10,
                                        n_row=10),
               "'number_digits' needs to be a single number.")
})

test_that("n_col format", {
  expect_error(check_inputs_surv_matrix(border_color="white",
                                        border_size=2,
                                        numbers=TRUE,
                                        number_color="white",
                                        number_size=3,
                                        number_family="sans",
                                        number_fontface="serif",
                                        number_digits=3,
                                        fixed_t=good_fixed_t,
                                        horizon=good_horizon,
                                        n_col="10",
                                        n_row=10),
               "'n_col' needs to be a single number.")
})

test_that("n_row format", {
  expect_error(check_inputs_surv_matrix(border_color="white",
                                        border_size=2,
                                        numbers=TRUE,
                                        number_color="white",
                                        number_size=3,
                                        number_family="sans",
                                        number_fontface="serif",
                                        number_digits=3,
                                        fixed_t=good_fixed_t,
                                        horizon=good_horizon,
                                        n_col=10,
                                        n_row="10"),
               "'n_row' needs to be a single number.")
})
