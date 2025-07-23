
## function to plot the restricted mean time lost as it evolves over values of
## the continuous variable
#' @importFrom rlang .data
#' @export
plot_surv_rmtl <- function(time, status, variable, group=NULL,
                           data, model, na.action=options()$na.action,
                           tau, horizon=NULL, custom_colors=NULL,
                           size=1, linetype="solid", alpha=1, color="black",
                           xlab=variable, ylab="Restricted Mean Time Lost",
                           title=NULL, subtitle=NULL,
                           legend.title=variable, legend.position="right",
                           gg_theme=ggplot2::theme_bw(),
                           facet_args=list(), ...) {
  requireNamespace("dplyr", quietly=TRUE)

  data <- use_data.frame(data)

  check_inputs_plots(time=time, status=status, variable=variable,
                     data=data, model=model, na.action=na.action,
                     horizon=horizon, fixed_t=NULL, max_t=Inf,
                     discrete=TRUE, panel_border=TRUE, t=1, tau=tau,
                     group=group)

  data <- prepare_inputdata(data=data, time=time, status=status,
                            variable=variable, model=model,
                            group=group, na.action=na.action)

  if (is.null(horizon)) {
    horizon <- seq(min(data[, variable]), max(data[, variable]),
                   length.out=100)
  }

  # get plotdata
  fixed_t <- c(0, sort(unique(data[, time][data[, status] >= 1])))
  plotdata <- curve_cont(data=data,
                         variable=variable,
                         group=group,
                         model=model,
                         horizon=horizon,
                         times=fixed_t,
                         na.action="na.fail",
                         cif=TRUE,
                         event_time=time,
                         event_status=status,
                         ...)

  # calculate RMTL values
  if (is.null(group)) {
    out <- cont_surv_auc(plotdata=plotdata, tau=tau)
  } else {
    group_levs <- levels(plotdata$group)
    out <- vector(mode="list", length=length(group_levs))
    for (i in seq_len(length(group_levs))) {
      temp <- plotdata[plotdata$group==group_levs[i], ]
      out_i <- cont_surv_auc(plotdata=temp, tau=tau)
      out_i$group <- group_levs[i]
      out[[i]] <- out_i
    }
    out <- dplyr::bind_rows(out)
  }

  # plot them
  p <- ggplot2::ggplot(out, ggplot2::aes(x=.data$cont, y=.data$auc,
                                         color=.data$tau))

  if (length(tau)==1) {
    p$mapping$colour <- NULL
    gg_line <- ggplot2::geom_line(linewidth=size, linetype=linetype,
                                  alpha=alpha, color=color)
  } else {
    gg_line <- ggplot2::geom_line(linewidth=size, linetype=linetype,
                                  alpha=alpha)
  }

  p <- p + gg_line +
    ggplot2::labs(x=xlab, y=ylab, title=title, subtitle=subtitle,
                  fill=legend.title) +
    gg_theme +
    ggplot2::theme(legend.position=legend.position)

  if (length(tau) > 1 & !is.null(custom_colors)) {
    p <- p + ggplot2::scale_colour_manual(values=custom_colors)
  }
  # facet plot by factor variable
  if (!is.null(group)) {
    facet_args$facets <- stats::as.formula("~ group")
    facet_obj <- do.call(ggplot2::facet_wrap, facet_args)
    p <- p + facet_obj
  }

  return(p)
}
