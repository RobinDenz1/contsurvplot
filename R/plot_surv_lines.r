
## function to simply draw many survival curves or CIFs, one for each x-value
#' @importFrom rlang .data
#' @export
plot_surv_lines <- function(time, status, variable, group=NULL, data, model,
                            cif=FALSE, conf_int=FALSE, conf_level=0.95,
                            n_boot=300, na.action=options()$na.action,
                            horizon=NULL, fixed_t=NULL, max_t=Inf,
                            discrete=TRUE, custom_colors=NULL,
                            start_color="blue", end_color="red",
                            size=1, linetype="solid", alpha=1,
                            xlab="Time", ylab="Survival Probability",
                            title=NULL, subtitle=NULL,
                            legend.title=variable, legend.position="right",
                            gg_theme=ggplot2::theme_bw(), facet_args=list(),
                            ci_alpha=0.4, kaplan_meier=FALSE, km_size=0.5,
                            km_linetype="solid", km_alpha=1,
                            km_color="black", km_ci=FALSE,
                            km_ci_type="plain", km_ci_level=0.95,
                            km_ci_alpha=0.4, ...) {

  data <- use_data.frame(data)

  check_inputs_plots(time=time, status=status, variable=variable,
                     data=data, model=model, na.action=na.action,
                     horizon=horizon, fixed_t=fixed_t, max_t=max_t,
                     discrete=discrete, panel_border=TRUE, t=1, tau=1,
                     group=group)

  data <- prepare_inputdata(data=data, time=time, status=status,
                            variable=variable, model=model,
                            group=group, na.action=na.action)

  if (is.null(fixed_t)) {
    fixed_t <- c(0, sort(unique(data[, time][data[, status]==1])))
  }
  if (is.null(horizon)) {
    horizon <- seq(min(data[, variable]), max(data[, variable]), length.out=12)
  }

  # only show up to max_t
  fixed_t <- fixed_t[fixed_t <= max_t]

  # get plotdata
  plotdata <- curve_cont(data=data,
                         variable=variable,
                         model=model,
                         group=group,
                         horizon=horizon,
                         times=fixed_t,
                         na.action="na.fail",
                         cif=cif,
                         event_time=time,
                         event_status=status,
                         conf_int=conf_int,
                         conf_level=conf_level,
                         n_boot=n_boot,
                         ...)
  # plot it
  if (!discrete) {
    # create enough colors
    colgrad_fun <- grDevices::colorRampPalette(c(start_color, end_color))
    colgrad <- colgrad_fun(length(horizon))
  } else {
    plotdata$cont <- as.factor(plotdata$cont)
  }

  # correct label
  if (cif & ylab=="Survival Probability") {
    ylab <- "Cumulative Incidence"
  }

  # plot it
  p <- ggplot2::ggplot(plotdata, ggplot2::aes(x=.data$time, y=.data$est,
                                              color=.data$cont,
                                              group=as.factor(.data$cont)))
  # add confidence intervals
  if (conf_int) {
    requireNamespace("pammtools")

    p <- p + pammtools::geom_stepribbon(ggplot2::aes(x=.data$time,
                                                     y=.data$est,
                                                     ymin=.data$ci_lower,
                                                     ymax=.data$ci_upper,
                                                     fill=.data$cont),
                                        alpha=ci_alpha, inherit.aes=FALSE)
  }

  p <- p +
    ggplot2::geom_step(linewidth=size, linetype=linetype, alpha=alpha) +
    ggplot2::labs(x=xlab, y=ylab, title=title, subtitle=subtitle,
                  fill=legend.title, color=legend.title) +
    gg_theme +
    ggplot2::theme(legend.position=legend.position)

  # add continuous of discrete color scale
  if (!discrete) {
    p <- p + ggplot2::scale_color_gradient(low=start_color, high=end_color)
  } else if (!is.null(custom_colors)) {
    p <- p + ggplot2::scale_colour_manual(values=custom_colors)
  }
  # add kaplan-meier estimates
  if (kaplan_meier) {
    km_dat <- get_kaplan_meier(time=time, status=status, group=group,
                               data=data, conf_int=km_ci,
                               conf_type=km_ci_type, conf_level=km_ci_level,
                               cif=cif)
    if (km_ci) {
      requireNamespace("pammtools")

      p <- p + pammtools::geom_stepribbon(data=km_dat,
                                          ggplot2::aes(x=.data$time,
                                                       y=.data$est,
                                                       ymin=.data$ci_lower,
                                                       ymax=.data$ci_upper),
                                          fill=km_color, alpha=km_ci_alpha,
                                          inherit.aes=FALSE)
    }
    p <- p + ggplot2::geom_step(data=km_dat, ggplot2::aes(x=.data$time,
                                                          y=.data$est),
                                linewidth=km_size, color=km_color,
                                alpha=km_alpha, linetype=km_linetype,
                                inherit.aes=FALSE)
  }
  # facet plot by factor variable
  if (!is.null(group)) {
    facet_args$facets <- stats::as.formula("~ group")
    facet_obj <- do.call(ggplot2::facet_wrap, facet_args)
    p <- p + facet_obj
  }

  return(p)
}
