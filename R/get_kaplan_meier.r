
## wrapper function to get an overall kaplan-meier estimate
get_kaplan_meier <- function(time, status, group=NULL, data, conf_int=FALSE,
                             conf_type="plain", conf_level=0.95,
                             cif=FALSE, ...) {

  # get formula
  if (is.null(group)) {
    form <- stats::as.formula(paste0("survival::Surv(", time, ", ",
                                     status, ") ~ 1"))
  } else {
    form <- stats::as.formula(paste0("survival::Surv(", time, ", ",
                                     status, ") ~ ", group))
  }

  # estimate kaplan-meier
  survf <- survival::survfit(formula=form, data=data, se.fit=conf_int,
                             conf.int=conf_level, conf.type=conf_type, ...)

  km_dat <- data.frame(time=survf$time,
                       est=survf$surv)

  # add group variable, if specified
  if (!is.null(group)) {
    km_group <- c()
    for (strat in names(survf$strata)) {
      km_group <- c(km_group, rep(strat, survf$strata[strat]))
    }
    km_group <- gsub(paste0(group, "="), "", km_group)
    km_dat$group <- factor(km_group, levels=levels(data[, group]))
  }

  # add confidence interval
  if (conf_int) {
    km_dat$ci_lower <- survf$lower
    km_dat$ci_upper <- survf$upper
  }

  # add rows at t = 0
  if (is.null(group) & conf_int) {
    row_0 <- data.frame(time=0, est=1, ci_lower=1, ci_upper=1)
  } else if (is.null(group)) {
    row_0 <- data.frame(time=0, est=1)
  } else if (!is.null(group) & conf_int) {
    row_0 <- data.frame(time=0, est=1, group=levels(data[, group]),
                        ci_lower=1, ci_upper=1)
  } else {
    row_0 <- data.frame(time=0, est=1, group=levels(data[, group]))
  }
  km_dat <- rbind(row_0, km_dat)

  # turn to cif
  if (cif) {
    km_dat$est <- 1 - km_dat$est
    if (conf_int) {
      km_dat$ci_lower <- 1 - km_dat$ci_lower
      km_dat$ci_upper <- 1 - km_dat$ci_upper
    }
  }

  return(km_dat)
}
