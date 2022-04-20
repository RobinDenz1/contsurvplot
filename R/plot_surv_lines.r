
## function to simply draw many survival curves or CIFs, one for each x-value
#' @importFrom rlang .data
#' @export
plot_surv_lines <- function(time, status, variable, data, model,
                            cif=FALSE, na.action=options()$na.action,
                            horizon=NULL, fixed_t=NULL, max_t=Inf,
                            color_scale=TRUE, custom_colors=NULL,
                            start_color="blue", end_color="red",
                            size=0.5, linetype="solid", alpha=1,
                            xlab="Time", ylab="Survival Probability",
                            title=NULL, subtitle=NULL,
                            legend.title=variable, legend.position="right",
                            gg_theme=ggplot2::theme_bw(), ...) {

  check_inputs_plots(time=time, status=status, variable=variable,
                     data=data, model=model, na.action=na.action,
                     horizon=horizon, fixed_t=fixed_t, max_t=max_t,
                     color_scale=color_scale, panel_border=TRUE, t=1, tau=1)

  # perform na.action
  if (is.function(na.action)) {
    data <- na.action(data)
  } else {
    na.action <- get(na.action)
    data <- na.action(data)
  }

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
                         horizon=horizon,
                         times=fixed_t,
                         na.action="na.fail",
                         cif=cif,
                         ...)
  # plot it
  if (color_scale) {
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
                                              group=as.factor(.data$cont))) +
    ggplot2::geom_step(size=size, linetype=linetype, alpha=alpha) +
    ggplot2::labs(x=xlab, y=ylab, title=title, subtitle=subtitle,
                  fill=legend.title, color=legend.title) +
    gg_theme +
    ggplot2::theme(legend.position=legend.position)

  # add continuous of discrete color scale
  if (color_scale) {
    p <- p + ggplot2::scale_color_gradient(low=start_color, high=end_color)
  } else if (!is.null(custom_colors)) {
    p <- p + ggplot2::scale_colour_manual(values=custom_colors)
  }

  return(p)
}
