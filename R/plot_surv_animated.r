
## function to draw one survival curve or CIF, and animate the continuous
## variable either as a gif or a slider
#' @importFrom rlang .data
#' @export
plot_surv_animated <- function(time, status, variable, data, model,
                               cif=FALSE, na.action=options()$na.action,
                               horizon=NULL, fixed_t=NULL, max_t=Inf,
                               slider=TRUE,
                               size=1, color="black", linetype="solid", alpha=1,
                               xlab="Time", ylab="Survival Probability",
                               title=NULL, subtitle=NULL,
                               gg_theme=ggplot2::theme_bw(), ...) {

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
    horizon <- seq(min(data[, variable]), max(data[, variable]), length.out=40)
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

  p <- p + ggplot2::geom_step(size=size, color=color, linetype=linetype,
                              alpha=alpha) +
    ggplot2::labs(x=xlab, y=ylab, title=title, subtitle=subtitle) +
    gg_theme

  if (slider) {
    requireNamespace("plotly")

    return(plotly::ggplotly(p))
  } else {
    requireNamespace("gganimate")
    requireNamespace("transformr")

    if (is.null(title)) {
      title <- variable
    }

    p <- p + gganimate::transition_time(.data$cont) +
      ggplot2::labs(title=paste0(title, ": {frame_time}"))
  }

  return(p)
}
