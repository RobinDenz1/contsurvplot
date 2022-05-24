
## keep only the covariates needed for the analysis
## this has to be done in order to correctly use na.action
remove_unnecessary_covars <- function(data, time, status, variable,
                                      group, model) {

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
  needed_covars <- c(time, status, variable, model_vars, group)

  # remove duplicates
  needed_covars <- unique(needed_covars)

  # filter data
  data <- dplyr::select(data, dplyr::all_of(needed_covars))

  return(data)
}

## composite function to prepare the data for further use
prepare_inputdata <- function(data, time, status, variable, group, model,
                              na.action) {

  # correct data type
  if (!inherits(data, "data.frame")) {
    stop("'data' must be a data.frame object.")
  }

  # only data.frame methods, no tibbles etc.
  data <- as.data.frame(data)

  # keep only needed columns
  data <- remove_unnecessary_covars(data=data, time=time, status=status,
                                    variable=variable, model=model,
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
