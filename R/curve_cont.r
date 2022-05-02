
## A function to calculate g-formula probability estimates over
## a horizon of continuous values at multiple points in time
#' @importFrom foreach %dopar%
#' @export
curve_cont <- function(data, variable, model, horizon,
                       times, cause=1, cif=FALSE, n_cores=1,
                       na.action=options()$na.action, ...) {

  check_inputs_curve_cont(data=data, variable=variable, model=model,
                          horizon=horizon, times=times, cause=cause,
                          cif=cif, na.action=na.action)

  # perform na.action
  if (is.function(na.action)) {
    data <- na.action(data)
  } else {
    na.action <- get(na.action)
    data <- na.action(data)
  }

  ## calculate the needed data
  # using single core
  if (n_cores <= 1) {
    plotdata <- vector(mode="list", length=length(horizon))
    for (i in seq_len(length(horizon))) {

      data_temp <- data
      data_temp[, variable] <- horizon[i]

      est_x <- riskRegression::predictRisk(object=model,
                                           newdata=data_temp,
                                           times=times,
                                           cause=cause,
                                           ...)
      if (!cif) {
        est_x <- 1 - est_x
      }

      est_x <- apply(X=est_x, MARGIN=2, FUN=mean, na.rm=TRUE)
      row <- data.frame(time=times, est=est_x, cont=horizon[i])
      plotdata[[i]] <- row
    }
  # using parallel processing
  } else {
    requireNamespace("parallel")
    requireNamespace("doParallel")
    requireNamespace("foreach")

    cl <- parallel::makeCluster(n_cores, outfile="")
    doParallel::registerDoParallel(cl)
    pkgs <- (.packages())

    plotdata <- foreach::foreach(i=seq_len(length(horizon)),
                                 .packages=pkgs) %dopar% {
      data_temp <- data
      data_temp[, variable] <- horizon[i]

      est_x <- riskRegression::predictRisk(object=model,
                                           newdata=data_temp,
                                           times=times,
                                           cause=cause,
                                           ...)
      if (!cif) {
        est_x <- 1 - est_x
      }

      est_x <- apply(X=est_x, MARGIN=2, FUN=mean, na.rm=TRUE)
      row <- data.frame(time=times, est=est_x, cont=horizon[i])
      row
    }
    parallel::stopCluster(cl)
  }
  plotdata <- dplyr::bind_rows(plotdata)

  return(plotdata)
}
