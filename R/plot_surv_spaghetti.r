
## function to simply draw many survival curves, one for each x-value
#' @importFrom rlang .data
#' @export
# TODO:
#   - add option to plot different curves as factor
#   - unit tests
#   - documentation
plot_surv_spaghetti <- function(time, status, variable, data, model,
                                horizon=NULL, fixed_t=NULL, max_t=Inf,
                                start_color="blue", end_color="red",
                                size=0.1, linetype="solid", alpha=1,
                                xlab="Time", ylab="Survival Probability",
                                title=NULL, subtitle=NULL,
                                legend.title=variable, legend.position="right",
                                gg_theme=ggplot2::theme_bw(), ...) {
  if (is.null(fixed_t)) {
    fixed_t <- c(0, sort(unique(data[, time][data[, status]==1])))
  }
  if (is.null(horizon)) {
    horizon <- seq(min(data[, variable]), max(data[, variable]), length.out=12)
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

  # plot it
  p <- ggplot2::ggplot(plotdata, ggplot2::aes(x=.data$time, y=.data$surv,
                                              color=.data$cont,
                                              group=as.factor(.data$cont))) +
    ggplot2::geom_step(size=size, linetype=linetype, alpha=alpha) +
    ggplot2::scale_color_gradient(low=start_color, high=end_color,
                                  name=variable) +
    ggplot2::labs(x=xlab, y=ylab, title=title, subtitle=subtitle,
                  fill=legend.title) +
    gg_theme +
    ggplot2::theme(legend.position=legend.position)

  return(p)
}
