
## new method to visualize the effect of a continuous variable on the
## survival or failure probability
#' @importFrom rlang .data
#' @export
plot_surv_area <- function(time, status, variable, data, model,
                           cif=FALSE, na.action=options()$na.action,
                           horizon=NULL, fixed_t=NULL, max_t=Inf,
                           start_color="blue", end_color="red",
                           discrete=FALSE, bins=ifelse(discrete, 10, 40),
                           alpha=1, xlab="Time", ylab="Survival Probability",
                           title=NULL, subtitle=NULL,
                           legend.title=variable, legend.position="right",
                           gg_theme=ggplot2::theme_bw(),
                           ...) {

  check_inputs_plots(time=time, status=status, variable=variable,
                     data=data, model=model, na.action=na.action,
                     horizon=horizon, fixed_t=fixed_t, max_t=max_t,
                     discrete=TRUE, panel_border=TRUE, t=1, tau=1)

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
    horizon <- seq(min(data[, variable]), max(data[, variable]),
                   length.out=bins+1)
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

  # create enough colors
  colgrad_fun <- grDevices::colorRampPalette(c(start_color, end_color))
  colgrad <- colgrad_fun(length(horizon)-1)

  # initialize plot
  p <- ggplot2::ggplot(plotdata, ggplot2::aes(x=.data$time, y=.data$est,
                                              color=.data$cont))

  # # NOTE: Invisible lines/bars are added here just to get the correct legend
  if (discrete) {
    fake_horizon <- as.factor(horizon[seq_len(length(horizon)-1)])
    levels(fake_horizon) <- get_bin_labels(horizon)

    fake_dat <- data.frame(x=0, y=0, col=fake_horizon)
    p <- p + ggplot2::geom_bar(data=fake_dat,
                               ggplot2::aes(x=.data$x,
                                            y=.data$y,
                                            fill=.data$col),
                               stat="identity",
                               inherit.aes=FALSE,
                               alpha=1) +
      ggplot2::scale_fill_manual(values=colgrad, name=legend.title) +
      ggplot2::guides(
        fill=ggplot2::guide_legend(override.aes=list(alpha=alpha)))

  } else {
    p <- p + ggplot2::geom_step(alpha=0) +
      ggplot2::scale_color_gradient(low=start_color, high=end_color)
  }

  # one-by-one add area between curves
  for (i in 1:(length(horizon)-1)) {
    plotdata_temp <- data.frame(time=plotdata$time[plotdata$cont==horizon[i]],
                                ymin=plotdata$est[plotdata$cont==horizon[i]],
                                ymax=plotdata$est[plotdata$cont==horizon[i+1]])

    p <- p + pammtools::geom_stepribbon(data=plotdata_temp,
                                        ggplot2::aes(ymin=.data$ymin,
                                                     ymax=.data$ymax,
                                                     x=.data$time),
                                        inherit.aes=FALSE,
                                        alpha=alpha,
                                        color=colgrad[i],
                                        fill=colgrad[i])
  }

  # correct label
  if (cif & ylab=="Survival Probability") {
    ylab <- "Cumulative Incidence"
  }

  # more stuff for plot
  p  <- p + gg_theme +
    ggplot2::labs(x=xlab, y=ylab, title=title, subtitle=subtitle,
                  color=legend.title, legend.position=legend.position)

  return(p)
}

## small helper function to create legend labels from horizon
get_bin_labels <- function(vec) {

  bins <- character(length=length(vec)-1)
  for (i in seq_len(length(vec)-1)) {
    label <- paste0(vec[i], " - ", vec[i+1])
    bins[i] <- label
  }
  return(bins)
}