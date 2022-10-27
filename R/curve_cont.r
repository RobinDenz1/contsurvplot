
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

## get one bootstrap analysis using a recursion call
curve_cont.boot <- function(i, data, variable, model, horizon,
                            times, group, cause, cif, contrast,
                            reference, ref_value, event_time,
                            event_status, ...) {
  # draw sample
  indices <- sample(x=rownames(data), size=nrow(data), replace=TRUE)
  boot_samp <- data[indices, ]

  # update model
  model <- stats::update(model, data=boot_samp)

  # perform g-computation
  out_i <- suppressWarnings({
    curve_cont(data=boot_samp, variable=variable,model=model,
               horizon=horizon, times=times, group=group,
               cause=cause, cif=cif, contrast=contrast,
               reference=reference, ref_value=ref_value,
               event_time=event_time, event_status=event_status,
               conf_int=FALSE, n_boot=300,
               n_cores=1, na.action="na.fail", ...)
    })
  out_i$boot_id <- i

  return(out_i)
}

## A function to calculate g-formula probability estimates over
## a horizon of continuous values at multiple points in time
#' @importFrom foreach %dopar%
#' @export
curve_cont <- function(data, variable, model, horizon,
                       times, group=NULL, cause=1, cif=FALSE,
                       contrast="none", reference="km", ref_value=NULL,
                       event_time=NULL, event_status=NULL,
                       conf_int=FALSE, conf_level=0.95,
                       n_boot=300, n_cores=1,
                       na.action=options()$na.action,
                       return_boot=FALSE, ...) {

  data <- use_data.frame(data)

  check_inputs_curve_cont(data=data, variable=variable, group=group,
                          model=model, horizon=horizon, times=times,
                          cause=cause, cif=cif, na.action=na.action,
                          contrast=contrast, reference=reference,
                          ref_value=ref_value, event_time=event_time,
                          event_status=event_status)

  data <- prepare_inputdata(data=data, time=event_time, status=event_status,
                            variable=variable, group=group,
                            model=model, na.action=na.action)

  if (is.null(group)) {
    group_levs <- NA
  } else {
    group_levs <- levels(data[, group])
  }

  ## calculate the needed data
  if (conf_int) {

    # get overall estimates
    plotdata <- curve_cont(data=data, variable=variable, model=model,
                           horizon=horizon, times=times, group=group,
                           cause=cause, cif=cif, contrast=contrast,
                           reference=reference, ref_value=ref_value,
                           event_time=event_time, event_status=event_status,
                           conf_int=FALSE, n_cores=1, na.action=na.action, ...)

    # perform bootstrapping (with or without multicore processing)
    if (n_cores > 1) {
      requireNamespace("parallel")
      requireNamespace("doParallel")

      # silence devtools::check()
      time <- cont <- est <- NULL

      cl <- parallel::makeCluster(n_cores, outfile="")
      doParallel::registerDoParallel(cl)
      pkgs <- (.packages())
      parallel::clusterCall(cl, function(pkgs){
        (invisible(lapply(pkgs, FUN=library, character.only=TRUE)))},
        pkgs=pkgs)

      bootdata <- parallel::parLapply(X=seq(1, n_boot), fun=curve_cont.boot,
                                      data=data, variable=variable, model=model,
                                      horizon=horizon, times=times, group=group,
                                      cause=cause, cif=cif, contrast=contrast,
                                      reference=reference, ref_value=ref_value,
                                      event_time=event_time,
                                      event_status=event_status,cl=cl, ...)
      parallel::stopCluster(cl)

    } else {
      bootdata <- lapply(X=seq(1, n_boot), FUN=curve_cont.boot,
                         data=data, variable=variable, model=model,
                         horizon=horizon, times=times, group=group,
                         cause=cause, cif=cif, contrast=contrast,
                         reference=reference, ref_value=ref_value,
                         event_time=event_time, event_status=event_status,
                         ...)
    }
    bootdata <- dplyr::bind_rows(bootdata)

    # if specified, directly return the individual bootstrap data
    if (return_boot) {
      return(bootdata)
    }

    if (!is.null(group)) {
      bootdata <- bootdata %>%
        dplyr::group_by(time, cont, group)
    } else {
      bootdata <- bootdata %>%
        dplyr::group_by(time, cont)
    }

    # get bootstrapped confidence intervals
    bootdata <- bootdata %>%
      dplyr::summarise(se=stats::sd(est, na.rm=TRUE),
                       ci_lower=stats::quantile(est,
                                                probs=(1-conf_level)/2,
                                                na.rm=TRUE),
                       ci_upper=stats::quantile(est,
                                                probs=1-((1-conf_level)/2),
                                                na.rm=TRUE),
                       .groups="drop_last")

    # put together
    if (!is.null(group)) {
      plotdata <- plotdata[order(plotdata$time, plotdata$cont,
                                 plotdata$group), ]
      bootdata <- bootdata[order(bootdata$time, bootdata$cont,
                                 bootdata$group), ]
    } else {
      plotdata <- plotdata[order(plotdata$time, plotdata$cont), ]
      bootdata <- bootdata[order(bootdata$time, bootdata$cont), ]
    }
    plotdata$se <- bootdata$se
    plotdata$ci_lower <- bootdata$ci_lower
    plotdata$ci_upper <- bootdata$ci_upper


  # using a single core
  } else if (n_cores <= 1) {
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

  # if specified, calculate contrasts instead of returning the value-specific
  # counterfactual estimates
  if (contrast!="none" & !conf_int) {
    # get reference values
    if (reference=="km") {
      ref <- get_kaplan_meier(time=event_time, status=event_status,
                              group=group, data=data, conf_int=FALSE,
                              conf_type="plain", conf_level=0.95,
                              cif=cif, fixed_t=times)
      colnames(ref)[colnames(ref)=="est"] <- "ref"
    } else {
      ref <- curve_cont(data=data, variable=variable, model=model, times=times,
                        horizon=ref_value, group=group, cause=cause, cif=cif,
                        conf_int=FALSE, na.action=na.action, contrast="none",
                        ...)
      colnames(ref)[colnames(ref)=="est"] <- "ref"
      ref$cont <- NULL
    }

    # merge reference to data
    if (is.null(group)) {
      id_vars <- c("time")
    } else {
      id_vars <- c("time", "group")
    }
    plotdata <- merge(plotdata, ref, by=id_vars, all.x=TRUE)

    # calculate contrast
    if (contrast=="diff") {
      plotdata$est <- plotdata$ref - plotdata$est
    } else if (contrast=="ratio") {
      plotdata$est <- plotdata$ref / plotdata$est
    }
    plotdata$ref <- NULL
  }

  return(plotdata)
}
