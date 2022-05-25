
## function to plot the survival or failure probability of a fixed point in time
## as a function of a continuous variable
#' @importFrom rlang .data
#' @export
plot_surv_at_t <- function(time, status, variable, group=NULL, data, model,
                           cif=FALSE, na.action=options()$na.action,
                           t, horizon=NULL,
                           size=1, linetype="solid", alpha=1,
                           xlab=variable, ylab="Survival Probability at t",
                           title=NULL, subtitle=NULL,
                           legend.title="t", legend.position="right",
                           gg_theme=ggplot2::theme_bw(),
                           facet_args=list(), ...) {

  data <- use_data.frame(data)

  check_inputs_plots(time=time, status=status, variable=variable,
                     data=data, model=model, na.action=na.action,
                     horizon=horizon, fixed_t=NULL, max_t=Inf,
                     discrete=TRUE, panel_border=TRUE, t=t, tau=1,
                     group=group)

  data <- prepare_inputdata(data=data, time=time, status=status,
                            variable=variable, model=model,
                            group=group, na.action=na.action)

  if (is.null(horizon)) {
    horizon <- seq(min(data[, variable]), max(data[, variable]),
                   length.out=100)
  }

  # get plotdata
  plotdata <- curve_cont(data=data,
                         variable=variable,
                         group=group,
                         model=model,
                         horizon=horizon,
                         times=t,
                         na.action="na.fail",
                         cif=cif,
                         ...)
  plotdata$time <- as.factor(plotdata$time)

  # plot them
  p <- ggplot2::ggplot(plotdata, ggplot2::aes(x=.data$cont, y=.data$est,
                                              color=.data$time))

  if (length(t)==1) {
    p$mapping$colour <- NULL
  }

  # correct label
  if (cif & ylab=="Survival Probability at t") {
    ylab <- "Cumulative Incidence at t"
  }

  p <- p + ggplot2::geom_line(size=size, linetype=linetype, alpha=alpha) +
    ggplot2::labs(x=xlab, y=ylab, title=title, subtitle=subtitle,
                  fill=legend.title, color=legend.title) +
    gg_theme +
    ggplot2::theme(legend.position=legend.position)

  # facet plot by factor variable
  if (!is.null(group)) {
    facet_args$facets <- stats::as.formula("~ group")
    facet_obj <- do.call(ggplot2::facet_wrap, facet_args)
    p <- p + facet_obj
  }

  return(p)
}
