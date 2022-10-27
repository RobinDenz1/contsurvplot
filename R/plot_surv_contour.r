
## A contour plot of time vs. continuous covariate with color grading
## showing the survival or failure probability
#' @importFrom rlang .data
#' @export
plot_surv_contour <- function(time, status, variable, group=NULL, data, model,
                              cif=FALSE, na.action=options()$na.action,
                              horizon=NULL, fixed_t=NULL, max_t=Inf,
                              size=0.1, linetype="solid", alpha=1,
                              bins=NULL, binwidth=NULL, breaks=NULL,
                              custom_colors=NULL,
                              xlab="Time", ylab=variable,
                              title=NULL, subtitle=NULL,
                              legend.title="S(t)", legend.position="right",
                              gg_theme=ggplot2::theme_bw(), facet_args=list(),
                              panel_border=FALSE, axis_dist=0,
                              ...) {

  data <- use_data.frame(data)

  check_inputs_plots(time=time, status=status, variable=variable,
                     data=data, model=model, na.action=na.action,
                     horizon=horizon, fixed_t=fixed_t, max_t=max_t,
                     discrete=TRUE, panel_border=panel_border, t=1, tau=1,
                     group=group)

  data <- prepare_inputdata(data=data, time=time, status=status,
                            variable=variable, model=model,
                            group=group, na.action=na.action)

  if (is.null(fixed_t)) {
    fixed_t <- seq(min(data[, time]), max(data[, time]), length.out=100)
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
                         group=group,
                         horizon=horizon,
                         times=fixed_t,
                         na.action="na.fail",
                         cif=cif,
                         event_time=time,
                         event_status=status,
                         ...)

  # correct label
  if (cif & legend.title=="S(t)") {
    legend.title <- "F(t)"
  }

  # plot it
  p <- ggplot2::ggplot(plotdata, ggplot2::aes(x=.data$time, y=.data$cont,
                                              z=.data$est)) +
    ggplot2::geom_contour_filled(size=size, alpha=alpha, bins=bins,
                                 binwidth=binwidth, breaks=breaks) +
    ggplot2::labs(x=xlab, y=ylab, title=title, subtitle=subtitle,
                  fill=legend.title) +
    gg_theme +
    ggplot2::theme(legend.position=legend.position) +
    ggplot2::scale_x_continuous(expand=c(axis_dist, axis_dist)) +
    ggplot2::scale_y_continuous(expand=c(axis_dist, axis_dist))

  if (!panel_border) {
    p <- p + ggplot2::theme(panel.border=ggplot2::element_blank())
  }
  if (!is.null(custom_colors)) {
    p <- p + ggplot2::scale_fill_manual(values=custom_colors)
  }
  # facet plot by factor variable
  if (!is.null(group)) {
    facet_args$facets <- stats::as.formula("~ group")
    facet_obj <- do.call(ggplot2::facet_wrap, facet_args)
    p <- p + facet_obj
  }
  return(p)
}
