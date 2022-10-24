
## keep only the covariates needed for the analysis
## this has to be done in order to correctly use na.action
remove_unnecessary_covars <- function(data, time, status, variable,
                                      group, event_time, event_status,
                                      model) {

  # extract variables from outcome model
  if (inherits(model, c("coxph", "mexhaz"))) {
    model_vars <- all.vars(model$formula)
  } else if (inherits(model, c("CauseSpecificCox", "FGR", "aalen",
                               "cox.aalen", "flexsurvreg",
                               "pecCforest", "prodlim",
                               "psm", "randomForest",
                               "riskRegression", "selectCox",
                               "glm", "ols", "rfsrc",
                               "penfitS3", "gbm",
                               "singleEventCB", "fcrr",
                               "comprisk"))) {
    model_vars <- all.vars(model$call$formula)
  } else if (inherits(model, "pecRpart")) {
    model_vars <- all.vars(model$rpart$terms)
  } else if (inherits(model, "ranger")) {
    model_vars <- all.vars(model$call[[2]])
  } else {
    model_vars <- NULL
  }

  # covariates that are always needed
  needed_covars <- c(time, status, variable, model_vars, group, event_time,
                     event_status)

  # remove duplicates
  needed_covars <- unique(needed_covars)

  # filter data
  data <- dplyr::select(data, dplyr::all_of(needed_covars))

  return(data)
}

## composite function to prepare the data for further use
prepare_inputdata <- function(data, time, status, variable, group,
                              event_time=NULL, event_status=NULL, model,
                              na.action) {

  # keep only needed columns
  data <- remove_unnecessary_covars(data=data, time=time, status=status,
                                    variable=variable, event_time=event_time,
                                    event_status=event_status, model=model,
                                    group=group)

  # perform na.action
  if (is.function(na.action)) {
    data <- na.action(data)
  } else {
    na.action <- get(na.action)
    data <- na.action(data)
  }

  if (nrow(data)==0) {
    stop("There is no data left after removing the missing values.")
  }

  return(data)
}

## use only data.frame methods, no tibbles etc.
use_data.frame <- function(data) {
  # correct data type
  if (!inherits(data, "data.frame")) {
    stop("'data' must be a data.frame object.")
  } else {
    data <- as.data.frame(data)
  }
  return(data)
}

## takes a value x at which to read from the step function
## and step function data from which to read it
read_from_step_function <- function(x, data, est="surv", time="time") {

  # keep only data with non-missing est
  data <- data[which(!is.na(data[, est])), ]

  # no extrapolation
  if (x > max(data[, time])) {
    return(NA)
  }

  # otherwise get value
  check <- data[which(data[, time] <= x), ]
  if (nrow(check)==0) {
    if (est=="surv") {
      val <- 1
    } else if (est=="cif") {
      val <- 0
    } else {
      val <- NA
    }
  } else {
    val <- check[, est][which(check[, time]==max(check[, time]))][1]
  }
  return(val)
}

## calculate exact integral of a step function
stepfun_integral <- function(x, y) {
  area <- 0
  for (i in seq_len((length(x)-1))) {
    x1 <- x[i]
    x2 <- x[i+1]
    rect_area <- (x2 - x1) * y[i]
    area <- area + rect_area
  }
  return(area)
}
