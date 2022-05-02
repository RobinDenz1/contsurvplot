
## function to plot survival time quantiles as they evolve over values of
## the continuous variable
# TODO: is there a "full" curve and how would i get that?
#' @importFrom rlang .data
#' @export
plot_surv_quantiles <- function(time, status, variable, data, model,
                                na.action=options()$na.action,
                                p=0.5, horizon=NULL,
                                size=1, linetype="solid", alpha=1,
                                custom_colors=NULL, single_color=NULL,
                                xlab=variable, ylab="Survival Time Quantile",
                                title=NULL, subtitle=NULL,
                                legend.title=variable, legend.position="right",
                                gg_theme=ggplot2::theme_bw(), ...) {
  requireNamespace("adjustedCurves")

  check_inputs_plots(time=time, status=status, variable=variable,
                     data=data, model=model, na.action=na.action,
                     horizon=horizon, fixed_t=NULL, max_t=Inf,
                     discrete=TRUE, panel_border=TRUE, t=1, tau=1)

  # perform na.action
  if (is.function(na.action)) {
    data <- na.action(data)
  } else {
    na.action <- get(na.action)
    data <- na.action(data)
  }

  if (is.null(horizon)) {
    horizon <- seq(min(data[, variable]), max(data[, variable]), length.out=40)
  }

  # get plotdata
  fixed_t <- c(0, sort(unique(data[, time][data[, status]==1])))
  plotdata <- curve_cont(data=data,
                         variable=variable,
                         model=model,
                         horizon=horizon,
                         times=fixed_t,
                         na.action="na.fail",
                         ...)

  # use adjustedCurves package to calculate survival time quantiles
  plotdata$group <- as.factor(plotdata$cont)
  plotdata$surv <- plotdata$est
  fake_adjsurv <- list(adjsurv=plotdata)
  class(fake_adjsurv) <- "adjustedsurv"

  surv_q <- adjustedCurves::adjusted_surv_quantile(fake_adjsurv, p=p,
                                                   conf_int=FALSE,
                                                   interpolation="steps")
  surv_q$group <- as.numeric(as.character(surv_q$group))
  surv_q$p <- as.factor(surv_q$p)

  # plot them
  plt <- ggplot2::ggplot(surv_q, ggplot2::aes(x=.data$group, y=.data$q_surv,
                                              color=.data$p, group=.data$p))

  if (length(p)==1) {
    plt$mapping$colour <- NULL
  }

  if (!is.null(single_color)) {
    gg_lines <- ggplot2::geom_line(size=size, linetype=linetype, alpha=alpha,
                                   color=single_color)
  } else {
    gg_lines <- ggplot2::geom_line(size=size, linetype=linetype, alpha=alpha)
  }

  plt <- plt + gg_lines +
    ggplot2::labs(x=xlab, y=ylab, title=title, subtitle=subtitle,
                  fill=legend.title) +
    gg_theme +
    ggplot2::theme(legend.position=legend.position)

  if (is.null(single_color) & !is.null(custom_colors)) {
    plt <- plt + ggplot2::scale_colour_manual(values=custom_colors)
  }
  return(plt)
}
