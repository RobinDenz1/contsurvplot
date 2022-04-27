
## check if horizon is correctly specified
check_horizon <- function(horizon, data, variable) {

  if (!(is.null(horizon) | (length(horizon)>=1 && is.numeric(horizon)))) {
    stop("'horizon' must be a numeric vector with at least one value or NULL.")
  } else if (!is.null(horizon) && anyNA(horizon)) {
    stop("Missing values in 'horizon' are not allowed.")
  } else if (!is.null(horizon) && min(horizon) < min(data[, variable],
                                                     na.rm=TRUE)) {
    stop("Some values in 'horizon' are smaller than the minimum observed",
         " in 'data', which is not allowed.")
  } else if (!is.null(horizon) && max(horizon) > max(data[, variable],
                                                     na.rm=TRUE)) {
    stop("Some values in 'horizon' are bigger than the maximum observed",
         " in 'data', which is not allowed.")
  }
}

## Check inputs of all plot functions
check_inputs_plots <- function(time, status, variable, data, model,
                               na.action, horizon, fixed_t, max_t,
                               discrete, panel_border, t, tau) {
  # correct data type
  if (!inherits(data, "data.frame")) {
    stop("'data' must be a data.frame object.")
  }

  # correct input type in time, status, variable
  if (!(length(time)==1 && is.character(time))) {
    stop("'time' must be a single character string specifying a",
         " variable in 'data'.")
  } else if (!(length(status)==1 && is.character(status))) {
    stop("'status' must be a single character string specifying a",
         " variable in 'data'.")
  } else if (!(length(variable)==1 && is.character(variable))) {
    stop("'variable' must be a single character string specifying a",
         " variable in 'data'.")
  }

  # time, status, variable in data
  if (!time %in% colnames(data)) {
    stop(time, " is not a valid column name in 'data'.")
  } else if (!status %in% colnames(data)) {
    stop(status, " is not a valid column name in 'data'.")
  } else if (!variable %in% colnames(data)) {
    stop(variable, " is not a valid column name in 'data'.")
  }

  # time, status, variable correct type in data
  if (!is.numeric(data[, time])) {
    stop("The column specified by the 'time' argument must be numeric.")
  } else if (!(is.numeric(data[, status]) | is.logical(data[, status]))) {
    stop("The column specified by the 'status' argument must be numeric or",
         " logical.")
  } else if (!is.numeric(data[, variable])) {
    stop("The column specified by the 'variable' argument must be numeric.")
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
                                    times, cause, cif, na.action) {
  # correct data type
  if (!inherits(data, "data.frame")) {
    stop("'data' must be a data.frame object.")
  }

  # correct variable
  if (!(length(variable)==1 && is.character(variable))) {
    stop("'variable' must be a single character string specifying a",
         " variable in 'data'.")
  } else if (!variable %in% colnames(data)) {
    stop(variable, " is not a valid column name in 'data'.")
  } else if (!is.numeric(data[, variable])) {
    stop("The column specified by the 'variable' argument must be numeric.")
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
}
