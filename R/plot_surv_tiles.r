
## Survival heatmap using only a discrete number of tiles,
## but with underlying smooth estimation
# TODO:
#   - allow group parameter
#' @importFrom rlang .data
#' @export
plot_surv_tiles <- function(time, status, variable, data, model,
                            cif=FALSE, na.action=options()$na.action,
                            horizon=NULL, fixed_t=NULL, max_t=Inf,
                            n_col=10, n_row=10,
                            start_color="red", end_color="blue",
                            alpha=1, xlab="Time", ylab=variable,
                            title=NULL, subtitle=NULL,
                            legend.title="S(t)", legend.position="none",
                            gg_theme=ggplot2::theme_bw(),
                            panel_border=FALSE, axis_dist=0,
                            border_color="white", border_size=0.5,
                            numbers=TRUE, number_color="white",
                            number_size=3, number_family="sans",
                            number_fontface="plain", number_digits=2,
                            ...) {
  # silence devtools::check()
  rect_id <- cont <- est <- NULL

  data <- use_data.frame(data)

  # standard input checks
  check_inputs_plots(time=time, status=status, variable=variable,
                     data=data, model=model, na.action=na.action,
                     horizon=horizon, fixed_t=fixed_t, max_t=max_t,
                     discrete=TRUE, panel_border=panel_border, t=1, tau=1,
                     group=NULL)

  # further special input checks
  check_inputs_tiles(border_color=border_color, border_size=border_size,
                     numbers=numbers, number_color=number_color,
                     number_size=number_size, number_family=number_family,
                     number_fontface=number_fontface,
                     number_digits=number_digits, fixed_t=fixed_t,
                     horizon=horizon, n_col=n_col, n_row=n_row)

  data <- prepare_inputdata(data=data, time=time, status=status,
                            variable=variable, model=model,
                            group=NULL, na.action=na.action)

  if (is.null(fixed_t)) {
    fixed_t <- seq(min(data[, time]), max(data[, time]), length.out=100)
  }
  if (is.null(horizon)) {
    horizon <- seq(min(data[, variable]), max(data[, variable]), length.out=100)
  }

  # final input checks for n_col, n_row
  if (n_col > length(fixed_t)) {
    stop("'n_col' must be smaller than length(fixed_t). Decrease n_col or",
         " increase the number of points in time used in the estimation.")
  }
  if (n_row > length(horizon)) {
    stop("'n_row' must be smaller than length(horizon). Decrease n_row or",
         " increase the number of values in horizon used in the estimation.")
  }

  # only show up to max_t
  fixed_t <- fixed_t[fixed_t <= max_t]

  # get plotdata
  plotdata <- curve_cont(data=data,
                         variable=variable,
                         model=model,
                         group=NULL,
                         horizon=horizon,
                         times=fixed_t,
                         na.action="na.fail",
                         cif=cif,
                         ...)

  # transform plotdata
  plotdata$time_cut <- cut(plotdata$time, n_col)
  plotdata$cont_cut <- cut(plotdata$cont, n_row)
  plotdata$rect_id <- paste(plotdata$time_cut, plotdata$cont_cut)

  plotdata <- plotdata %>%
    dplyr::group_by(rect_id) %>%
    dplyr::summarise(xmin=min(time),
                     xmax=max(time),
                     ymin=min(cont),
                     ymax=max(cont),
                     est=mean(est))

  # close gap between tiles
  gap_x <- fixed_t[2] - fixed_t[1]
  gap_y <- abs(horizon[2] - horizon[1])

  plotdata$xmax <- plotdata$xmax + gap_x
  plotdata$ymax <- plotdata$ymax + gap_y

  if (numbers) {
    # get coordinates for text
    plotdata$x_text <- rowMeans(plotdata[, c("xmax", "xmin")])
    plotdata$y_text <- rowMeans(plotdata[, c("ymax", "ymin")])

    # round the numbers
    plotdata$label <- round(plotdata$est, number_digits)
  }

  # correct label
  if (cif & legend.title=="S(t)") {
    legend.title <- "F(t)"
  }

  # plot it
  p <- ggplot2::ggplot(plotdata, ggplot2::aes(fill=.data$est, xmin=.data$xmin,
                                              xmax=.data$xmax, ymin=.data$ymin,
                                              ymax=.data$ymax)) +
    ggplot2::geom_rect(color=border_color, alpha=alpha, size=border_size) +
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
  if (numbers) {
    p <- p + ggplot2::geom_text(ggplot2::aes(x=.data$x_text, y=.data$y_text,
                                             label=.data$label),
                                color=number_color,
                                size=number_size,
                                family=number_family,
                                fontface=number_fontface)
  }
  return(p)
}
