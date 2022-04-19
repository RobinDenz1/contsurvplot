
## function to plot restricted mean survival times as they evolve over values of
## the continuous variable
#' @importFrom rlang .data
#' @export
plot_surv_rmst <- function(time, status, variable, data, outcome_model,
                           tau, horizon=NULL,
                           size=1, linetype="solid", alpha=1,
                           xlab=variable, ylab="Restricted Mean Survival Time",
                           title=NULL, subtitle=NULL,
                           legend.title=variable, legend.position="right",
                           gg_theme=ggplot2::theme_bw(), ...) {

  if (is.null(horizon)) {
    horizon <- seq(min(data[, variable]), max(data[, variable]),
                   length.out=100)
  }

  # get plotdata
  fixed_t <- c(0, sort(unique(data[, time][data[, status]==1])))
  plotdata <- surv_curve_cont(data=data,
                              variable=variable,
                              outcome_model=outcome_model,
                              horizon=horizon,
                              times=fixed_t,
                              ...)

  # use adjustedCurves package to calculate RMST values
  plotdata$group <- as.factor(plotdata$cont)
  plotdata$cont <- NULL
  fake_adjsurv <- list(adjsurv=plotdata)
  class(fake_adjsurv) <- "adjustedsurv"

  out <- vector(mode="list", length=length(tau))
  for (i in seq_len(length(tau))) {
    rmst <- adjustedCurves::adjusted_rmst(fake_adjsurv, from=0, to=tau[i],
                                          conf_int=FALSE)
    rmst$group <- as.numeric(as.character(rmst$group))
    rmst$tau <- tau[i]

    out[[i]] <- rmst
  }
  out <- dplyr::bind_rows(out)
  out$tau <- as.factor(out$tau)

  # plot them
  p <- ggplot2::ggplot(out, ggplot2::aes(x=group, y=rmst, color=tau))

  if (length(tau)==1) {
    plt$mapping$colour <- NULL
  }

  p <- p + ggplot2::geom_line(size=size, linetype=linetype, alpha=alpha) +
    ggplot2::labs(x=xlab, y=ylab, title=title, subtitle=subtitle,
                  fill=legend.title) +
    gg_theme +
    ggplot2::theme(legend.position=legend.position)

  return(p)
}
