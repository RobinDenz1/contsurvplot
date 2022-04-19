
## A heatmap of time vs. continuous covariate with color grading
## showing the survival probability
#' @importFrom rlang .data
#' @export
plot_surv_heatmap <- function(time, status, variable, data, model,
                              horizon=NULL, fixed_t=NULL, max_t=Inf,
                              start_color=NULL, end_color=NULL,
                              size=0.1, alpha=1, xlab="Time", ylab=variable,
                              title=NULL, subtitle=NULL,
                              legend.title="S(t)", legend.position="right",
                              gg_theme=ggplot2::theme_bw(),
                              panel_border=FALSE, axis_dist=0, ...) {
  if (is.null(fixed_t)) {
    fixed_t <- seq(min(data[, time]), max(data[, time]), length.out=100)
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

  # plot it
  p <- ggplot2::ggplot(plotdata, ggplot2::aes(x=.data$time, y=.data$cont,
                                              fill=.data$surv)) +
    ggplot2::geom_tile(size=size, alpha=alpha) +
    ggplot2::labs(x=xlab, y=ylab, title=title, subtitle=subtitle,
                  fill=legend.title) +
    gg_theme +
    ggplot2::theme(legend.position=legend.position) +
    ggplot2::scale_x_continuous(expand=c(axis_dist, axis_dist)) +
    ggplot2::scale_y_continuous(expand=c(axis_dist, axis_dist))

  if (!is.null(start_color) & !is.null(end_color)) {
    p <- p + ggplot2::scale_fill_gradient(low=start_color, high=end_color)
  }
  if (!panel_border) {
    p <- p + ggplot2::theme(panel.border=ggplot2::element_blank())
  }

  return(p)
}
