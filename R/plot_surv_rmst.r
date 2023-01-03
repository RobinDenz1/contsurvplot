
## small function to estimate rmtl from plotdata
cont_surv_auc <- function(plotdata, tau) {
  plotdata$group <- as.factor(plotdata$cont)
  plotdata$surv <- plotdata$est
  plotdata$est <- NULL
  plotdata$cont <- NULL

  levs <- levels(plotdata$group)

  out <- vector(mode="list", length=length(tau)*length(levs))
  count <- 0
  for (i in seq_len(length(tau))) {
    for (j in seq_len(length(levs))) {
      count <- count + 1

      data_j <- plotdata[plotdata$group==levs[j], ]

      # constrain function end
      latest <- read_from_step_function(tau[i], data_j, est="surv", time="time")
      data_j <- data_j[data_j$time <= tau[i], ]
      data_j$group <- NULL

      if (!tau[i] %in% data_j$time) {
        temp <- data.frame(time=tau[i],
                           surv=latest)
        data_j <- rbind(data_j, temp)
      }

      # calculate rmst
      area <- stepfun_integral(x=data_j$time, y=data_j$surv)

      row <- data.frame(group=as.numeric(as.character(levs[j])),
                        rmst=area,
                        tau=tau[i])
      out[[count]] <- row
    }
  }
  out <- dplyr::bind_rows(out)
  out$tau <- as.factor(out$tau)

  return(out)
}

## function to plot restricted mean survival times as they evolve over values of
## the continuous variable
#' @importFrom rlang .data
#' @export
plot_surv_rmst <- function(time, status, variable, group=NULL,
                           data, model, na.action=options()$na.action,
                           tau, horizon=NULL, custom_colors=NULL,
                           size=1, linetype="solid", alpha=1, color="black",
                           xlab=variable, ylab="Restricted Mean Survival Time",
                           title=NULL, subtitle=NULL,
                           legend.title=variable, legend.position="right",
                           gg_theme=ggplot2::theme_bw(),
                           facet_args=list(), ...) {
  requireNamespace("dplyr")

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
  fixed_t <- c(0, sort(unique(data[, time][data[, status]==1])))
  plotdata <- curve_cont(data=data,
                         variable=variable,
                         group=group,
                         model=model,
                         horizon=horizon,
                         times=fixed_t,
                         na.action="na.fail",
                         event_time=time,
                         event_status=status,
                         ...)

  # calculate RMST values
  if (is.null(group)) {
    out <- cont_surv_auc(plotdata=plotdata, tau=tau)
  } else {
    group_levs <- levels(plotdata$group)
    out <- vector(mode="list", length=length(group_levs))
    for (i in seq_len(length(group_levs))) {
      temp <- plotdata[plotdata$group==group_levs[i], ]
      out_i <- cont_surv_auc(plotdata=temp, tau=tau)
      out_i$facet_var <- group_levs[i]
      out[[i]] <- out_i
    }
    out <- dplyr::bind_rows(out)
  }

  # plot them
  p <- ggplot2::ggplot(out, ggplot2::aes(x=.data$group, y=.data$rmst,
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
    facet_args$facets <- stats::as.formula("~ facet_var")
    facet_obj <- do.call(ggplot2::facet_wrap, facet_args)
    p <- p + facet_obj
  }

  return(p)
}
