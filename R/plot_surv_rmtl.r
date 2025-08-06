
## function to plot the restricted mean time lost as it evolves over values of
## the continuous variable
#' @importFrom rlang .data
#' @export
plot_surv_rmtl <- function(time, status, variable, group=NULL, cause=1,
                           data, model, conf_int=FALSE, conf_level=0.95,
                           n_boot=300, na.action=options()$na.action,
                           tau, horizon=NULL, custom_colors=NULL,
                           size=1, linetype="solid", alpha=1, color="black",
                           xlab=variable, ylab="Restricted Mean Time Lost",
                           title=NULL, subtitle=NULL,
                           legend.title="tau", legend.position="right",
                           gg_theme=ggplot2::theme_bw(),
                           facet_args=list(), ci_alpha=0.4, ...) {
  requireNamespace("dplyr", quietly=TRUE)

  data <- use_data.frame(data)

  check_inputs_plots(time=time, status=status, variable=variable,
                     data=data, model=model, na.action=na.action,
                     horizon=horizon, fixed_t=NULL, max_t=Inf,
                     discrete=TRUE, panel_border=TRUE, t=1, tau=tau,
                     group=group)

  data <- prepare_inputdata(data=data, time=time, status=status,
                            variable=variable, model=model,
                            group=group, na.action=na.action)

  # get plotdata
  out <- curve_cont_auc(
    data=data,
    variable=variable,
    model=model,
    time=time,
    status=status,
    horizon=horizon,
    group=group,
    cause=cause,
    cif=TRUE,
    conf_int=conf_int,
    conf_level=conf_level,
    n_boot=n_boot,
    tau=tau,
    ...
  )

  # plot them
  p <- ggplot2::ggplot(out, ggplot2::aes(x=.data$cont, y=.data$auc,
                                         color=.data$tau))

  if (conf_int & length(tau)==1) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(x=.data$cont,
                                      y=.data$auc,
                                      ymin=.data$ci_lower,
                                      ymax=.data$ci_upper),
                         alpha=ci_alpha, inherit.aes=FALSE,
                         fill=color)
  } else if (conf_int) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(x=.data$cont,
                                      y=.data$auc,
                                      ymin=.data$ci_lower,
                                      ymax=.data$ci_upper,
                                      fill=.data$tau),
                         alpha=ci_alpha, inherit.aes=FALSE)
  }

  if (length(tau)==1) {
    p$mapping$colour <- NULL
    gg_line <- ggplot2::geom_line(linewidth=size, linetype=linetype,
                                  alpha=alpha, color=color)
  } else {
    gg_line <- ggplot2::geom_line(linewidth=size, linetype=linetype,
                                  alpha=alpha)
  }

  p <- p + gg_line +
    ggplot2::labs(x=xlab, y=ylab, title=title, subtitle=subtitle,
                  fill=legend.title, color=legend.title) +
    gg_theme +
    ggplot2::theme(legend.position=legend.position)

  if (length(tau) > 1 & !is.null(custom_colors)) {
    p <- p + ggplot2::scale_colour_manual(values=custom_colors)
  }
  # facet plot by factor variable
  if (!is.null(group)) {
    facet_args$facets <- stats::as.formula("~ group")
    facet_obj <- do.call(ggplot2::facet_wrap, facet_args)
    p <- p + facet_obj
  }

  return(p)
}
