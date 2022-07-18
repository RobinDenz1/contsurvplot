
## small function to obtain the survival time quantiles from
## estimated plotdata
cont_surv_quantiles <- function(plotdata, p) {

  plotdata$group <- as.factor(plotdata$cont)
  plotdata$surv <- plotdata$est
  fake_adjsurv <- list(adjsurv=plotdata)
  class(fake_adjsurv) <- "adjustedsurv"

  surv_q <- adjustedCurves::adjusted_surv_quantile(fake_adjsurv, p=p,
                                                   conf_int=FALSE,
                                                   interpolation="steps")
  surv_q$group <- as.numeric(as.character(surv_q$group))
  surv_q$p <- as.factor(surv_q$p)

  return(surv_q)
}

## function to plot survival time quantiles as they evolve over values of
## the continuous variable
#' @importFrom rlang .data
#' @export
plot_surv_quantiles <- function(time, status, variable, group=NULL, data, model,
                                na.action=options()$na.action,
                                p=0.5, horizon=NULL,
                                size=1, linetype="solid", alpha=1,
                                custom_colors=NULL, single_color=NULL,
                                xlab=variable, ylab="Survival Time Quantile",
                                title=NULL, subtitle=NULL,
                                legend.title=variable, legend.position="right",
                                gg_theme=ggplot2::theme_bw(), facet_args=list(),
                                ...) {
  requireNamespace("adjustedCurves")

  data <- use_data.frame(data)

  check_inputs_plots(time=time, status=status, variable=variable,
                     data=data, model=model, na.action=na.action,
                     horizon=horizon, fixed_t=NULL, max_t=Inf,
                     discrete=TRUE, panel_border=TRUE, t=1, tau=1,
                     group=group)

  data <- prepare_inputdata(data=data, time=time, status=status,
                            variable=variable, model=model,
                            group=group, na.action=na.action)

  if (is.null(horizon)) {
    horizon <- seq(min(data[, variable]), max(data[, variable]), length.out=40)
  }

  # get plotdata
  fixed_t <- c(0, sort(unique(data[, time][data[, status]==1])))
  plotdata <- curve_cont(data=data,
                         variable=variable,
                         model=model,
                         group=group,
                         horizon=horizon,
                         times=fixed_t,
                         na.action="na.fail",
                         ...)

  # use the adjustedCurves package to calculate survival time quantiles
  if (is.null(group)) {
    surv_q <- cont_surv_quantiles(plotdata=plotdata, p=p)
  } else {
    group_levs <- levels(plotdata$group)
    surv_q <- vector(mode="list", length=length(group_levs))
    for (i in seq_len(length(group_levs))) {
      temp <- plotdata[plotdata$group==group_levs[i], ]
      out_i <- cont_surv_quantiles(plotdata=temp, p=p)
      out_i$facet_var <- group_levs[i]
      surv_q[[i]] <- out_i
    }
    surv_q <- dplyr::bind_rows(surv_q)
  }

  # plot them
  plt <- ggplot2::ggplot(surv_q, ggplot2::aes(x=.data$group, y=.data$q_surv,
                                              color=.data$p, group=.data$p))

  if (length(p)==1) {
    plt$mapping$colour <- NULL
  }

  if (!is.null(single_color)) {
    gg_lines <- ggplot2::geom_step(size=size, linetype=linetype, alpha=alpha,
                                   color=single_color)
  } else {
    gg_lines <- ggplot2::geom_step(size=size, linetype=linetype, alpha=alpha)
  }

  plt <- plt + gg_lines +
    ggplot2::labs(x=xlab, y=ylab, title=title, subtitle=subtitle,
                  fill=legend.title) +
    gg_theme +
    ggplot2::theme(legend.position=legend.position)

  if (is.null(single_color) & !is.null(custom_colors)) {
    plt <- plt + ggplot2::scale_colour_manual(values=custom_colors)
  }
  # facet plot by factor variable
  if (!is.null(group)) {
    facet_args$facets <- stats::as.formula("~ facet_var")
    facet_obj <- do.call(ggplot2::facet_wrap, facet_args)
    plt <- plt + facet_obj
  }
  return(plt)
}
