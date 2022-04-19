
## new method to visualize the effect of a continuous variable on the
## survival probability
# maybe use geom_raster() instead?
# or https://stackoverflow.com/questions/53397131/gradient-fill-in-ggplot2 ?
# add median survival line
#' @importFrom rlang .data
#' @export
plot_surv_contcurve <- function(time, status, variable, data, model,
                                horizon=NULL, fixed_t=NULL, max_t=Inf,
                                start_color="blue", end_color="red", alpha=1,
                                xlab="Time", ylab="Survival Probability",
                                title=NULL, subtitle=NULL,
                                legend.title=variable, legend.position="right",
                                gg_theme=ggplot2::theme_bw(),
                                ...) {
  if (is.null(fixed_t)) {
    fixed_t <- c(0, sort(unique(data[, time][data[, status]==1])))
  }
  if (is.null(horizon)) {
    horizon <- seq(min(data[, variable]), max(data[, variable]), length.out=40)
  }

  # only show up to max_t
  fixed_t <- fixed_t[fixed_t <= max_t]

  # get plotdata
  plotdata <- surv_curve_cont(data=data,
                              variable=variable,
                              model=model,
                              horizon=horizon,
                              times=fixed_t,
                              ...)

  # create enough colors
  colgrad_fun <- colorRampPalette(c(start_color, end_color))
  colgrad <- colgrad_fun(length(horizon))

  # initialize plot
  # NOTE: Invisible lines are added here just to get the correct legend
  p <- ggplot2::ggplot(plotdata, ggplot2::aes(x=time, y=surv, color=cont)) +
    ggplot2::geom_step(alpha=0) +
    ggplot2::scale_color_gradient(low=start_color, high=end_color)

  # one-by-one add area between curves
  for (i in 1:(length(horizon)-1)) {
    plotdata_temp <- data.frame(time=plotdata$time[plotdata$cont==horizon[i]],
                                ymin=plotdata$surv[plotdata$cont==horizon[i]],
                                ymax=plotdata$surv[plotdata$cont==horizon[i+1]])

    p <- p + pammtools::geom_stepribbon(data=plotdata_temp,
                                        ggplot2::aes(ymin=.data$ymin,
                                                     ymax=.data$ymax,
                                                     x=.data$time),
                                        inherit.aes=FALSE,
                                        alpha=alpha,
                                        color=colgrad[i],
                                        fill=colgrad[i])
  }

  # more stuff for plot
  p  <- p + gg_theme +
    ggplot2::labs(x=xlab, y=ylab, title=title, subtitle=subtitle,
                  color=legend.title, legend.position=legend.position)

  return(p)
}
