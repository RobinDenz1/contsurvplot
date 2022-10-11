
## function to draw one survival curve or CIF, and animate the continuous
## variable either as a gif or a slider
#' @importFrom rlang .data
#' @export
plot_surv_animated <- function(time, status, variable, group=NULL, data, model,
                               cif=FALSE, conf_int=FALSE, conf_level=0.95,
                               n_boot=300, na.action=options()$na.action,
                               horizon=NULL, fixed_t=NULL, max_t=Inf,
                               slider=TRUE,
                               size=1, color="black", linetype="solid", alpha=1,
                               xlab="Time", ylab="Survival Probability",
                               title=NULL, subtitle=NULL,
                               gg_theme=ggplot2::theme_bw(),
                               facet_args=list(), ci_alpha=0.4, ...) {

  data <- use_data.frame(data)

  check_inputs_plots(time=time, status=status, variable=variable,
                     data=data, model=model, na.action=na.action,
                     horizon=horizon, fixed_t=fixed_t, max_t=max_t,
                     discrete=TRUE, panel_border=TRUE, t=1, tau=1,
                     group=group)

  data <- prepare_inputdata(data=data, time=time, status=status,
                            variable=variable, model=model,
                            group=group, na.action=na.action)

  if (is.null(fixed_t)) {
    fixed_t <- c(0, sort(unique(data[, time][data[, status]==1])))
  }
  if (is.null(horizon)) {
    horizon <- seq(min(data[, variable]), max(data[, variable]), length.out=40)
  }

  # only show up to max_t
  fixed_t <- fixed_t[fixed_t <= max_t]

  # get plotdata
  plotdata <- curve_cont(data=data,
                         variable=variable,
                         group=group,
                         model=model,
                         horizon=horizon,
                         times=fixed_t,
                         na.action="na.fail",
                         cif=cif,
                         conf_int=conf_int,
                         conf_level=conf_level,
                         n_boot=n_boot,
                         ...)

  # correct label
  if (cif & ylab=="Survival Probability") {
    ylab <- "Cumulative Incidence"
  }

  # plot it
  if (slider) {
    p <- ggplot2::ggplot(plotdata, ggplot2::aes(x=.data$time, y=.data$est,
                                                frame=.data$cont))
  } else {
    p <- ggplot2::ggplot(plotdata, ggplot2::aes(x=.data$time, y=.data$est))
  }

  if (conf_int & slider) {
    stop("Showing confidence intervals is currently not supported when using",
         " slider=TRUE.")
  } else if (conf_int) {
    requireNamespace("pammtools")

    p <- p + pammtools::geom_stepribbon(ggplot2::aes(x=.data$time,
                                                     y=.data$est,
                                                     ymin=.data$ci_lower,
                                                     ymax=.data$ci_upper),
                                        alpha=ci_alpha, inherit.aes=FALSE,
                                        fill=color)
  }

  p <- p + ggplot2::geom_step(size=size, color=color, linetype=linetype,
                              alpha=alpha) +
    ggplot2::labs(x=xlab, y=ylab, title=title, subtitle=subtitle) +
    gg_theme

  # facet plot by factor variable
  if (!is.null(group)) {
    facet_args$facets <- stats::as.formula("~ group")
    facet_obj <- do.call(ggplot2::facet_wrap, facet_args)
    p <- p + facet_obj
  }

  if (slider) {
    requireNamespace("plotly")

    return(plotly::ggplotly(p))
  } else {
    requireNamespace("gganimate")
    requireNamespace("transformr")

    if (is.null(title)) {
      title <- variable
    }

    p <- p + gganimate::transition_manual(frames=.data$cont) +
      ggplot2::labs(title=paste0(title, ": {frame}"))
  }

  return(p)
}
