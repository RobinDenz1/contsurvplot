
## calculates AUC values, possibly with confidence intervals
curve_cont_auc <- function(data, variable, model, time, status, horizon=NULL,
                           group=NULL, cause=1, cif=FALSE, conf_int=FALSE,
                           conf_level=0.95, n_boot=300, tau, ...) {

  boot_id <- cont <- auc <- NULL

  # default horizon
  if (is.null(horizon)) {
    horizon <- seq(min(data[, variable]), max(data[, variable]),
                   length.out=100)
  }

  # needed points in time
  fixed_t <- c(0, sort(unique(data[, time][data[, status] >= 1])))

  # get survival probabilities / CIFs
  plotdata <- curve_cont(data=data,
                         variable=variable,
                         group=group,
                         model=model,
                         horizon=horizon,
                         times=fixed_t,
                         na.action="na.fail",
                         event_time=time,
                         event_status=status,
                         cif=cif,
                         cause=cause,
                         conf_int=FALSE,
                         conf_level=0.95,
                         n_boot=300,
                         ...)

  # calculate AUC from it
  out <- get_auc_from_plotdata(plotdata=plotdata, tau=tau,
                               group=group)

  if (conf_int) {

    # get bootstrapped data
    plotdata_boot <- curve_cont(
      data=data,
      variable=variable,
      group=group,
      model=model,
      horizon=horizon,
      times=fixed_t,
      na.action="na.fail",
      event_time=time,
      event_status=status,
      cif=cif,
      cause=cause,
      conf_int=conf_int,
      conf_level=conf_level,
      n_boot=n_boot,
      return_boot=TRUE,
      ...
    )

    # calculate AUC for each bootstrap sample
    out_boot <- vector(mode="list", length=n_boot)
    for (i in seq_len(n_boot)) {
      plotdata_i <- subset(plotdata_boot, boot_id==i)
      plotdata_i$boot_id <- NULL
      out_boot[[i]] <- get_auc_from_plotdata(plotdata=plotdata_i, tau=tau,
                                             group=group)
    }
    out_boot <- dplyr::bind_rows(out_boot)

    # calculate bootstrap CI
    if (is.null(group)) {
      out_boot <- out_boot %>%
        dplyr::group_by(cont, tau) %>%
        dplyr::summarise(se=stats::sd(auc, na.rm=TRUE),
                         ci_lower=stats::quantile(auc,
                                                  probs=(1-conf_level)/2,
                                                  na.rm=TRUE),
                         ci_upper=stats::quantile(auc,
                                                  probs=1-((1-conf_level)/2),
                                                  na.rm=TRUE),
                         .groups="drop_last")
      by <- c("cont", "tau")
    } else {
      out_boot <- out_boot %>%
        dplyr::group_by(cont, tau, group) %>%
        dplyr::summarise(se=stats::sd(auc, na.rm=TRUE),
                         ci_lower=stats::quantile(auc,
                                                  probs=(1-conf_level)/2,
                                                  na.rm=TRUE),
                         ci_upper=stats::quantile(auc,
                                                  probs=1-((1-conf_level)/2),
                                                  na.rm=TRUE),
                         .groups="drop_last")
      by <- c("cont", "tau", "group")
    }

    # merge CI back to data
    out <- merge(out, out_boot, by=by, all.x=TRUE)
  }

  return(out)
}

## small function to estimate rmtl from plotdata
cont_surv_auc <- function(plotdata, tau) {

  plotdata$group <- as.factor(plotdata$cont)
  plotdata$surv <- plotdata$est
  plotdata$est <- NULL
  plotdata$cont <- NULL

  levs <- levels(plotdata$group)

  out <- vector(mode="list", length=length(tau)*length(levs))
  count <- 0
  for (i in seq_len(length(tau))) {
    for (j in seq_len(length(levs))) {
      count <- count + 1

      data_j <- plotdata[plotdata$group==levs[j], ]

      # constrain function end
      latest <- read_from_step_function(tau[i], data_j, est="surv", time="time")
      data_j <- data_j[data_j$time <= tau[i], ]
      data_j$group <- NULL

      if (!tau[i] %in% data_j$time) {
        temp <- data.frame(time=tau[i],
                           surv=latest)
        data_j <- rbind(data_j, temp)
      }

      # calculate rmst
      area <- stepfun_integral(x=data_j$time, y=data_j$surv)

      row <- data.frame(cont=as.numeric(as.character(levs[j])),
                        auc=area,
                        tau=tau[i])
      out[[count]] <- row
    }
  }
  out <- dplyr::bind_rows(out)
  out$tau <- as.factor(out$tau)

  return(out)
}

## calculate the AUC given estimated survival curves
get_auc_from_plotdata <- function(plotdata, tau, group) {

  if (is.null(group)) {
    out <- cont_surv_auc(plotdata=plotdata, tau=tau)
  } else {
    group_levs <- levels(plotdata$group)
    out <- vector(mode="list", length=length(group_levs))
    for (i in seq_len(length(group_levs))) {
      temp <- plotdata[plotdata$group==group_levs[i], ]
      out_i <- cont_surv_auc(plotdata=temp, tau=tau)
      out_i$group <- group_levs[i]
      out[[i]] <- out_i
    }
    out <- dplyr::bind_rows(out)
  }

  return(out)
}
