
## function to plot survival time quantiles as they evolve over values of
## the continuous variable
#' @importFrom rlang .data
#' @export
plot_surv_quantiles <- function(time, status, variable, data, model,
                                p=0.5, horizon=NULL,
                                size=1, linetype="solid", alpha=1,
                                xlab=variable, ylab="Survival Time Quantile",
                                title=NULL, subtitle=NULL,
                                legend.title=variable, legend.position="right",
                                gg_theme=ggplot2::theme_bw(), ...) {

  if (is.null(horizon)) {
    horizon <- seq(min(data[, variable]), max(data[, variable]), length.out=100)
  }

  # get plotdata
  fixed_t <- c(0, sort(unique(data[, time][data[, status]==1])))
  plotdata <- surv_curve_cont(data=data,
                              variable=variable,
                              model=model,
                              horizon=horizon,
                              times=fixed_t,
                              ...)

  # use adjustedCurves package to calculate survival time quantiles
  plotdata$group <- as.factor(plotdata$cont)
  fake_adjsurv <- list(adjsurv=plotdata)
  class(fake_adjsurv) <- "adjustedsurv"

  surv_q <- adjustedCurves::adjusted_surv_quantile(fake_adjsurv, p=p,
                                                   conf_int=FALSE,
                                                   interpolation="steps")
  surv_q$group <- as.numeric(as.character(surv_q$group))
  surv_q$p <- as.factor(surv_q$p)

  # plot them
  plt <- ggplot2::ggplot(surv_q, ggplot2::aes(x=group, y=q_surv, color=p))

  if (length(p)==1) {
    plt$mapping$colour <- NULL
  }

  plt <- plt + ggplot2::geom_step(size=size, linetype=linetype, alpha=alpha) +
    ggplot2::labs(x=xlab, y=ylab, title=title, subtitle=subtitle,
                  fill=legend.title) +
    gg_theme +
    ggplot2::theme(legend.position=legend.position)

  return(plt)
}
