
## a function to calculate g-formula survival probability estimates over
## a horizon of continuous values at multiple points in time
#' @export
surv_curve_cont <- function(data, variable, model, horizon,
                            times, ...) {

  # calculate the needed data
  plotdata <- vector(mode="list", length=length(horizon))
  for (i in 1:length(horizon)) {

    data_temp <- data
    data_temp[, variable] <- horizon[i]

    surv_x <- riskRegression::predictRisk(object=model,
                                          newdata=data_temp,
                                          times=times,
                                          cause=1,
                                          ...)
    surv_x <- 1 - surv_x

    surv_x <- apply(X=surv_x, MARGIN=2, FUN=mean, na.rm=TRUE)
    row <- data.frame(time=times, surv=surv_x, cont=horizon[i])
    plotdata[[i]] <- row
  }
  plotdata <- dplyr::bind_rows(plotdata)
  return(plotdata)
}
