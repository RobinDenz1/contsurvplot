
## a single iteration of the main loop in the curve_cont function
## put in a separate function to avoid code repetition due to
## code for parallel processing
one_iteration <- function(data, variable, cont_value, group, group_levs,
                          group_lev, cause, cif, times, model, ...) {
  data_temp <- data
  data_temp[, variable] <- cont_value

  if (!is.na(group_lev)) {
    data_temp[, group] <- factor(group_lev, levels=group_levs)
  }

  est_x <- riskRegression::predictRisk(object=model,
                                       newdata=data_temp,
                                       times=times,
                                       cause=cause,
                                       ...)
  if (!cif) {
    est_x <- 1 - est_x
  }

  est_x <- apply(X=est_x, MARGIN=2, FUN=mean, na.rm=TRUE)
  row <- data.frame(time=times, est=est_x, cont=cont_value, group=group_lev)

  return(row)
}

## A function to calculate g-formula probability estimates over
## a horizon of continuous values at multiple points in time
#' @importFrom foreach %dopar%
#' @export
curve_cont <- function(data, variable, model, horizon,
                       times, group=NULL, cause=1, cif=FALSE, n_cores=1,
                       na.action=options()$na.action, ...) {

  data <- use_data.frame(data)

  check_inputs_curve_cont(data=data, variable=variable, group=group,
                          model=model, horizon=horizon, times=times,
                          cause=cause, cif=cif, na.action=na.action)

  data <- prepare_inputdata(data=data, time=variable, status=variable,
                            variable=variable, group=group, model=model,
                            na.action=na.action)

  if (is.null(group)) {
    group_levs <- NA
  } else {
    group_levs <- levels(data[, group])
  }

  ## calculate the needed data
  # using single core
  if (n_cores <= 1) {
    plotdata <- vector(mode="list", length=length(horizon)*length(group_levs))
    count <- 1
    for (i in seq_len(length(horizon))) {
      for (j in seq_len(length(group_levs))) {
        plotdata[[count]] <- one_iteration(data=data,
                                           variable=variable,
                                           cont_value=horizon[i],
                                           group=group,
                                           group_levs=group_levs,
                                           group_lev=group_levs[j],
                                           cause=cause,
                                           cif=cif,
                                           times=times,
                                           model=model,
                                           ...)
        count <- count + 1
      }
    }
  # using parallel processing
  } else {
    requireNamespace("parallel")
    requireNamespace("doParallel")
    requireNamespace("foreach")

    cl <- parallel::makeCluster(n_cores, outfile="")
    doParallel::registerDoParallel(cl)
    pkgs <- (.packages())
    export_objs <- c("one_iteration")

    plotdata <- foreach::foreach(i=seq_len(length(horizon)),
                                 .packages=pkgs,
                                 .export=export_objs) %dopar% {
      out_i <- vector(mode="list", length=length(horizon))
      for (j in seq_len(length(group_levs))) {
        out_i[[j]] <- one_iteration(data=data,
                                    variable=variable,
                                    cont_value=horizon[i],
                                    group=group,
                                    group_levs=group_levs,
                                    group_lev=group_levs[j],
                                    cause=cause,
                                    cif=cif,
                                    times=times,
                                    model=model,
                                    ...)
      }
      dplyr::bind_rows(out_i)
    }
    parallel::stopCluster(cl)
  }
  plotdata <- dplyr::bind_rows(plotdata)

  if (is.null(group)) {
    plotdata$group <- NULL
  } else {
    plotdata$group <- factor(plotdata$group, levels=group_levs)
  }

  return(plotdata)
}
