
## function to plot the continuous survival area as a 3D surface
#' @importFrom dplyr %>%
#' @export
plot_surv_3Dsurface <- function(time, status, variable, data, model,
                                cif=FALSE, na.action=options()$na.action,
                                horizon=NULL, fixed_t=NULL, max_t=Inf,
                                interactive=FALSE,
                                xlab="Time", ylab="Survival Probability",
                                zlab=variable, ticktype="detailed",
                                theta=120, phi=20, col="green", shade=0.5,
                                ...) {
  requireNamespace("reshape2")

  data <- prepare_inputdata(data=data, time=time, status=status,
                            variable=variable, model=model,
                            group=NULL, na.action=na.action)

  check_inputs_plots(time=time, status=status, variable=variable,
                     data=data, model=model, na.action=na.action,
                     horizon=horizon, fixed_t=fixed_t, max_t=max_t,
                     discrete=TRUE, panel_border=TRUE, t=1, tau=1)

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
                         group=NULL,
                         model=model,
                         horizon=horizon,
                         times=fixed_t,
                         na.action="na.fail",
                         cif=cif,
                         ...)
  # transform
  plot_matrix <- t(reshape2::acast(plotdata, cont~time, value.var="est"))

  if (interactive) {
    requireNamespace("plotly")

    p <- plotly::plot_ly(x=as.numeric(colnames(plot_matrix)),
                         y=as.numeric(rownames(plot_matrix)),
                         z=plot_matrix) %>%
      plotly::add_surface() %>%
      plotly::layout(
        scene=list(
          xaxis=list(title=zlab),
          yaxis=list(title=xlab),
          zaxis=list(title=ylab)
        ))
    return(p)
  } else {
    p <- graphics::persp(x=as.numeric(colnames(plot_matrix)),
                         y=as.numeric(rownames(plot_matrix)),
                         z=t(plot_matrix),
                         xlab=zlab,
                         ylab=xlab,
                         zlab=ylab,
                         ticktype=ticktype,
                         theta=theta,
                         phi=phi,
                         col=col,
                         shade=shade)
    return(invisible(p))
  }
}
