
## new method to visualize the effect of a continuous variable on the
## survival or failure probability
#' @importFrom rlang .data
#' @export
plot_surv_area <- function(time, status, variable, group=NULL, data, model,
                           cif=FALSE, na.action=options()$na.action,
                           horizon=NULL, fixed_t=NULL, max_t=Inf,
                           start_color="blue",mid_color="white" ,end_color="red", alpha=1,
                           discrete=FALSE, bins=ifelse(discrete, 10, 40),
                           sep_lines=FALSE, sep_color="black", sep_size=0.1,
                           sep_linetype="solid", sep_alpha=alpha,
                           xlab="Time", ylab="Survival Probability",
                           title=NULL, subtitle=NULL,
                           legend.title=variable, legend.position="right",
                           gg_theme=ggplot2::theme_bw(), facet_args=list(),
                           label_digits=NULL, transition_size=0.01,
                           kaplan_meier=FALSE, km_size=0.5,
                           km_linetype="solid", km_alpha=1, km_color="black",
                           monotonic=TRUE, ...) {

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
    horizon <- seq(min(data[, variable]), max(data[, variable]),
                   length.out=bins+1)
  } else {
    horizon <- sort(horizon)
  }

  if (!is.null(group) & !monotonic) {
    stop("The 'group' argument cannot be used with monotonic=FALSE.")
  } else if (!monotonic & length(horizon) < 40) {
    warning("To obtain valid results when using monotonic=FALSE, the",
            " 'horizon' should contain at least 40 distinct values.")
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
                         ...)

  # partition into piecewise non-increasing/non-decreasing segments
  if (!monotonic) {
    plotdata_one_t <- subset(plotdata, time==fixed_t[2])
    plotdata_one_t <- plotdata_one_t[order(plotdata_one_t$cont),]

    turn_points <- plotdata_one_t$cont[find_turns(plotdata_one_t$est)]
    cut_points <- sort(unique(c(horizon[1], turn_points, max(horizon))))

    plotdata$partition <- cut(plotdata$cont, breaks=cut_points,
                              include.lowest=TRUE)
  }

  # create enough colors
  colgrad_fun <- grDevices::colorRampPalette(c(start_color, mid_color, end_color))
  colgrad <- colgrad_fun(length(horizon)-1)

  print(summary(plotdata$cont)) ##!test for legend_value skipping

  # initialize plot
  p <- ggplot2::ggplot(plotdata, ggplot2::aes(x=.data$time, y=.data$est,
                                              color=.data$cont))

  # # NOTE: Invisible lines/tiles are added here just to get the correct legend
  if (discrete) {
    fake_horizon <- as.factor(horizon[seq_len(length(horizon)-1)])
    levels(fake_horizon) <- get_bin_labels(horizon, digits=label_digits)

    fake_dat <- data.frame(x=horizon[1],
                           y=min(plotdata$est, na.rm=TRUE),
                           col=fake_horizon)
    p <- p + ggplot2::geom_tile(data=fake_dat,
                                ggplot2::aes(x=.data$x,
                                             y=.data$y,
                                             fill=.data$col),
                                inherit.aes=FALSE,
                                height=0.0001,
                                alpha=0) +
      ggplot2::scale_fill_manual(values=colgrad, name=legend.title) +
      ggplot2::guides(
        fill=ggplot2::guide_legend(override.aes=list(alpha=alpha)))

  } else {
    p <- p + ggplot2::geom_step(alpha=0) +
      ggplot2::scale_color_gradient2(low=start_color, mid =mid_color,high=end_color)+
      ggplot2::labs(color = "cont")
  }

  # one-by-one add area between curves
  for (i in 1:(length(horizon)-1)) {
    plotdata_temp <- data.frame(time=plotdata$time[plotdata$cont==horizon[i]],
                                ymin=plotdata$est[plotdata$cont==horizon[i]],
                                ymax=plotdata$est[plotdata$cont==horizon[i+1]])

    if (!is.null(group)) {
      plotdata_temp$group <- plotdata$group[plotdata$cont==horizon[i]]
    }
    if (!monotonic) {
      plotdata_temp$partition <- plotdata$partition[plotdata$cont==horizon[i]]
    }

    surv_segment <- pammtools::geom_stepribbon(data=plotdata_temp,
                                               ggplot2::aes(ymin=.data$ymin,
                                                            ymax=.data$ymax,
                                                            x=.data$time),
                                               inherit.aes=FALSE,
                                               alpha=alpha,
                                               fill=colgrad[i])
    if (!discrete & alpha==1) {
      surv_segment$aes_params$colour <- colgrad[i]
      surv_segment$aes_params$size <- transition_size
    }
    p <- p + surv_segment
  }

  if (sep_lines) {
    p <- p + ggplot2::geom_step(ggplot2::aes(group=as.factor(.data$cont)),
                                linewidth=sep_size, color=sep_color,
                                linetype=sep_linetype, alpha=sep_alpha)
  }

  # correct label
  if (cif & ylab=="Survival Probability") {
    ylab <- "Cumulative Incidence"
  }

  # more stuff for plot
  p  <- p + gg_theme +
    ggplot2::labs(x=xlab, y=ylab, title=title, subtitle=subtitle,
                  color=legend.title, legend.position=legend.position)

  # add kaplan-meier reference line, if specified
  if (kaplan_meier) {
    km_dat <- get_kaplan_meier(time=time, status=status, group=group,
                               data=data, conf_int=FALSE, cif=cif,
                               fixed_t=fixed_t)
    p <- p + ggplot2::geom_step(data=km_dat, linewidth=km_size, color=km_color,
                                alpha=km_alpha, linetype=km_linetype)
  }

  # facet plot by factor / partition variable
  if (!is.null(group) & monotonic) {
    form <- "~ group"
  } else if (is.null(group) & !monotonic) {
    form <- "~ partition"
  }

  if (!is.null(group) | !monotonic) {
    facet_args$facets <- stats::as.formula(form)
    facet_obj <- do.call(ggplot2::facet_wrap, facet_args)
    p <- p + facet_obj
  }

  return(p)
}

## small helper function to create legend labels from horizon
get_bin_labels <- function(vec, digits) {

  bins <- character(length=length(vec)-1)
  for (i in seq_len(length(vec)-1)) {
    if (is.null(digits)) {
      label <- paste0(vec[i], " - ", vec[i+1])
    } else {
      label <- paste0(round(vec[i], digits), " - ", round(vec[i+1], digits))
    }
    bins[i] <- label
  }
  return(bins)
}

## identify points at which a one-dimensional vector changes from being
## non-increasing to being non-decreasing
## requires x to be sorted by the variable of interest
find_turns <- function(x) {

  # loop over it once to identify monotonic segments
  monoto <- logical((length(x)-1))
  for (i in seq(2, length(x))) {
    prev <- x[i - 1]
    current <- x[i]

    if (current <= prev) {
      monoto[i] <- TRUE
    } else {
      monoto[i] <- FALSE
    }
  }

  # make vector always start with TRUE
  if (!monoto[1]) {
    monoto <- !monoto
  }

  # identify the turning points
  turn_points <- c()
  for (i in seq(2, length(monoto))) {
    prev <- monoto[i - 1]
    current <- monoto[i]

    if (prev != current) {
      turn_points <- c(turn_points, i-1)
    }
  }

  return(turn_points)
}
