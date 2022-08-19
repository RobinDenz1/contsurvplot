
## check if horizon is correctly specified
check_horizon <- function(horizon, data, variable) {

  if (!(is.null(horizon) | (length(horizon)>=1 && is.numeric(horizon)))) {
    stop("'horizon' must be a numeric vector with at least one value or NULL.")
  } else if (!is.null(horizon) && anyNA(horizon)) {
    stop("Missing values in 'horizon' are not allowed.")
  } else if (!is.null(horizon) && min(horizon) < min(data[, variable],
                                                     na.rm=TRUE)) {
    warning("Some values in 'horizon' are smaller than the minimum observed",
            " in 'data', which might lead to problems.")
  } else if (!is.null(horizon) && max(horizon) > max(data[, variable],
                                                     na.rm=TRUE)) {
    warning("Some values in 'horizon' are bigger than the maximum observed",
            " in 'data', which might lead to problems.")
  }
}

## Check inputs of all plot functions
check_inputs_plots <- function(time, status, variable, group, data, model,
                               na.action, horizon, fixed_t, max_t,
                               discrete, panel_border, t, tau) {

  # correct input type in time, status, variable, group
  if (!(length(time)==1 && is.character(time))) {
    stop("'time' must be a single character string specifying a",
         " variable in 'data'.")
  } else if (!(length(status)==1 && is.character(status))) {
    stop("'status' must be a single character string specifying a",
         " variable in 'data'.")
  } else if (!(length(variable)==1 && is.character(variable))) {
    stop("'variable' must be a single character string specifying a",
         " variable in 'data'.")
  } else if (!is.null(group) & !(length(group)==1 && is.character(group))) {
    stop("'group' must be a single character string specifying a",
         " factor variable in 'data' or NULL.")
  }

  # time, status, variable, group in data
  if (!time %in% colnames(data)) {
    stop(time, " is not a valid column name in 'data'.")
  } else if (!status %in% colnames(data)) {
    stop(status, " is not a valid column name in 'data'.")
  } else if (!variable %in% colnames(data)) {
    stop(variable, " is not a valid column name in 'data'.")
  } else if (!is.null(group) && !group %in% colnames(data)) {
    stop(group, " is not a valid column name in 'data'.")
  }

  # time, status, variable, group correct type in data
  if (!is.numeric(data[, time])) {
    stop("The column specified by the 'time' argument must be numeric.")
  } else if (!(is.numeric(data[, status]) | is.logical(data[, status]))) {
    stop("The column specified by the 'status' argument must be numeric or",
         " logical.")
  } else if (!is.numeric(data[, variable])) {
    stop("The column specified by the 'variable' argument must be numeric.")
  } else if (!is.null(group) && !is.factor(data[, group])) {
    stop("The column specified by the 'group' argument must be a factor.")
  }

  # a few more checks for the most popular case: coxph
  if (inherits(model, "coxph")) {
    # variable in formula
    if (!variable %in% all.vars(model$formula)) {
      stop("The 'variable' argument needs to be included as independent",
           " variable in the coxph model.")
    }
    # no missing coefficients
    if (anyNA(model$coefficients)) {
      stop("Estimation impossible due to missing or infinite coefficients",
           " in the coxph model.")
    }
  }

  # horizon
  check_horizon(horizon, data, variable)

  # fixed_t
  if (!(is.null(fixed_t) | (length(fixed_t)>1 && is.numeric(fixed_t)))) {
    stop("'fixed_t' must be a numeric vector with at least two values or NULL.")
  } else if (!is.null(fixed_t) && anyNA(fixed_t)) {
    stop("Missing values in 'fixed_t' are not allowed.")
  } else if (!is.null(fixed_t) && min(fixed_t) < 0) {
    stop("Some values in 'fixed_t' are smaller than 0, which is not allowed.")
  } else if (!is.null(fixed_t) && max(fixed_t) > max(data[, time],
                                                     na.rm=TRUE)) {
    stop("Some values in 'fixed_t' are bigger than the maximum observed",
         " time in 'data', which is not allowed.")
  }

  # na.action
  if (!(length(na.action)==1 && (is.function(na.action) |
                                 is.character(na.action)))) {
    stop("'na.action' must be a function or a single character string.")
  }

  # max_t
  if (!(length(max_t)==1 && (is.infinite(max_t) | is.numeric(max_t)))) {
    stop("'max_t' must be a single number.")
  }
  # discrete
  if (!(length(discrete)==1 && is.logical(discrete))) {
    stop("'discrete' must be either TRUE or FALSE.")
  }
  # panel_border
  if (!(length(panel_border)==1 && is.logical(panel_border))) {
    stop("'panel_border' must be either TRUE or FALSE.")
  }
  # t (plot_surv_at_t)
  if (!(length(t)>=1 && is.numeric(t))) {
    stop("'t' must be a numeric vector.")
  }
  # tau (plot_surv_rmst)
  if (!(length(tau)>=1 && is.numeric(tau))) {
    stop("'tau' must be a numeric vector.")
  }
}

## Check inputs for the curve_cont function
check_inputs_curve_cont <- function(data, variable, model, horizon,
                                    times, cause, cif, na.action, group) {
  # correct variable
  if (!(length(variable)==1 && is.character(variable))) {
    stop("'variable' must be a single character string specifying a",
         " variable in 'data'.")
  } else if (!variable %in% colnames(data)) {
    stop(variable, " is not a valid column name in 'data'.")
  } else if (!is.numeric(data[, variable])) {
    stop("The column specified by the 'variable' argument must be numeric.")
  }
  # correct group
  if (!is.null(group) && !(length(group)==1 && is.character(group))) {
    stop("'group' must be a single character string specifying a",
         " factor variable in 'data' or NULL.")
  } else if (!is.null(group) && !group %in% colnames(data)) {
    stop(group, " is not a valid column name in 'data'.")
  } else if (!is.null(group) && !is.factor(data[, group])) {
    stop("The column specified by the 'group' argument must be a factor.")
  }

  # horizon
  check_horizon(horizon, data, variable)

  # times
  if (!(length(times)>0 && is.numeric(times))) {
    stop("'times' must be a numeric vector with at least one value.")
  } else if (anyNA(times)) {
    stop("Missing values in 'times' are not allowed.")
  } else if (min(times) < 0) {
    stop("Some values in 'times' are smaller than 0, which is not allowed.")
  }

  # a few more checks for the most popular case: coxph
  if (inherits(model, "coxph")) {
    # no missing coefficients
    if (anyNA(model$coefficients)) {
      stop("Estimation impossible due to missing or infinite coefficients",
           " in the coxph model.")
    }
  }

  # check if needed variables were used in model
  if (!is.null(model$formula)) {
    all_vars <- all.vars(model$formula)
    # variable in formula
    if (!variable %in% all_vars) {
      stop("The 'variable' argument needs to be included as independent",
           " variable in the 'model' object.")
    # group in formula
    } else if (!is.null(group) && !group %in% all_vars) {
      stop("The 'group' argument needs to be included as independent",
           " variable in the 'model' object.")
    }
  }
}

## Check inputs for special parameters in plot_surv_tiles
check_inputs_surv_matrix <- function(border_color, border_size, numbers,
                                     number_color, number_size, number_family,
                                     number_fontface, number_digits, fixed_t,
                                     horizon, n_col, n_row) {

  # check if fixed_t is equally spaced
  if (!is.null(fixed_t)) {
    expected <- seq(fixed_t[1], fixed_t[length(fixed_t)],
                    length.out=length(fixed_t))
    if (!all(fixed_t==expected)) {
      stop("'fixed_t' needs to be an equally spaced numeric vector.",
           " For example, you could use seq(min, max, length.out=100).")
    }
  }

  # check if horizon is equally spaced
  if (!is.null(horizon)) {
    expected <- seq(horizon[1], horizon[length(horizon)],
                    length.out=length(horizon))
    if (!all(horizon==expected)) {
      stop("'horizon' needs to be an equally spaced numeric vector.",
           " For example, you could use seq(min, max, length.out=100).")
    }
  }

  # check other arguments
  if (!is.null(border_color) && !(length(border_color)==1 &&
                                  is.character(border_color))) {
    stop("'border_color' needs to be a single character string specifying",
         " the color of the rectangle borders or NULL.")
  } else if (!(length(border_size)==1 && is.numeric(border_size))) {
    stop("'border_size' needs to be a single number.")
  } else if (!(length(numbers)==1 && is.logical(numbers))) {
    stop("'numbers' must be either TRUE or FALSE.")
  } else if (!(length(number_color)==1 && is.character(number_color))) {
    stop("'number_color' needs to be a single character string specifying",
         " the color of the text inside the rectangles.")
  } else if (!(length(number_size)==1 && is.numeric(number_size))) {
    stop("'number_size' needs to be a single number.")
  } else if (!(length(number_family)==1 && is.character(number_family))) {
    stop("'number_family' needs to be a single character string.")
  } else if (!(length(number_fontface)==1 && is.character(number_fontface))) {
    stop("'number_fontface' needs to be a single character string.")
  } else if (!(length(number_digits)==1 && is.numeric(number_digits))) {
    stop("'number_digits' needs to be a single number.")
  } else if (!(length(n_col)==1 && is.numeric(n_col))) {
    stop("'n_col' needs to be a single number.")
  } else if (!(length(n_row)==1 && is.numeric(n_row))) {
    stop("'n_row' needs to be a single number.")
  }
}
