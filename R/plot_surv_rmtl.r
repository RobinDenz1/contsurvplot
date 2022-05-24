
## small function to estimate rmtl from plotdata
cont_surv_rmtl <- function(plotdata, tau) {
  plotdata$group <- as.factor(plotdata$cont)
  plotdata$cif <- plotdata$est
  plotdata$est <- NULL
  plotdata$cont <- NULL
  fake_adjcif <- list(adjcif=plotdata)
  class(fake_adjcif) <- "adjustedcif"

  out <- vector(mode="list", length=length(tau))
  for (i in seq_len(length(tau))) {
    rmtl <- adjustedCurves::adjusted_rmtl(fake_adjcif, from=0, to=tau[i],
                                          conf_int=FALSE)
    rmtl$group <- as.numeric(as.character(rmtl$group))
    rmtl$tau <- tau[i]

    out[[i]] <- rmtl
  }
  out <- dplyr::bind_rows(out)
  out$tau <- as.factor(out$tau)

  return(out)
}

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
  requireNamespace("adjustedCurves")
  requireNamespace("dplyr")

  data <- prepare_inputdata(data=data, time=time, status=status,
                            variable=variable, model=model,
                            group=group, na.action=na.action)

  check_inputs_plots(time=time, status=status, variable=variable,
                     data=data, model=model, na.action=na.action,
                     horizon=horizon, fixed_t=NULL, max_t=Inf,
                     discrete=TRUE, panel_border=TRUE, t=1, tau=tau)

  if (is.null(horizon)) {
    horizon <- seq(min(data[, variable]), max(data[, variable]),
                   length.out=100)
  }

  # get plotdata
  fixed_t <- c(0, sort(unique(data[, time][data[, status]==1])))
  plotdata <- curve_cont(data=data,
                         variable=variable,
                         group=group,
                         model=model,
                         horizon=horizon,
                         times=fixed_t,
                         na.action="na.fail",
                         cif=TRUE,
                         ...)

  # use adjustedCurves package to calculate RMTL values
  if (is.null(group)) {
    out <- cont_surv_rmtl(plotdata=plotdata, tau=tau)
  } else {
    group_levs <- levels(plotdata$group)
    out <- vector(mode="list", length=length(group_levs))
    for (i in seq_len(length(group_levs))) {
      temp <- plotdata[plotdata$group==group_levs[i], ]
      out_i <- cont_surv_rmtl(plotdata=temp, tau=tau)
      out_i$facet_var <- group_levs[i]
      out[[i]] <- out_i
    }
    out <- dplyr::bind_rows(out)
  }

  # plot them
  p <- ggplot2::ggplot(out, ggplot2::aes(x=.data$group, y=.data$rmtl,
                                         color=.data$tau))

  if (length(tau)==1) {
    p$mapping$colour <- NULL
    gg_line <- ggplot2::geom_line(size=size, linetype=linetype, alpha=alpha,
                                  color=color)
  } else {
    gg_line <- ggplot2::geom_line(size=size, linetype=linetype, alpha=alpha)
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
    facet_args$facets <- stats::as.formula("~ facet_var")
    facet_obj <- do.call(ggplot2::facet_wrap, facet_args)
    p <- p + facet_obj
  }

  return(p)
}
