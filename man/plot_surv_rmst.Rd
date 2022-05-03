\name{plot_surv_rmst}
\alias{plot_surv_rmst}

\title{
Plot the Effect of a Continuous Variable on the Restricted Mean Survival Time
}
\description{
Using a previously fit time-to-event model, this function plots the restricted mean survival time (RMST) as a function of a continuous variable. Internally uses the \pkg{adjustedCurves} package to calculate the RMST.
}
\usage{
plot_surv_rmst(time, status, variable, data, model,
               na.action=options()$na.action,
               tau, horizon=NULL, custom_colors=NULL,
               size=1, linetype="solid", alpha=1, color="black",
               xlab=variable, ylab="Restricted Mean Survival Time",
               title=NULL, subtitle=NULL,
               legend.title=variable, legend.position="right",
               gg_theme=ggplot2::theme_bw(), ...)
}
\arguments{
  \item{time}{
A single character string specifying the time-to-event variable. Needs to be a valid column name of a variable in \code{data}.
  }
  \item{status}{
A single character string specifying the status variable, indicating if a person has experienced an event or not. Needs to be a valid column name of a variable in \code{data}.
  }
  \item{variable}{
A single character string specifying the continuous variable of interest, for which the survival curves should be estimated. This variable has to be contained in the \code{data.frame} that is supplied to the \code{data} argument.
  }
  \item{data}{
A \code{data.frame} containing all required variables.
  }
  \item{model}{
A model describing the time-to-event process (such as an \code{coxph} model). Needs to include \code{variable} as an independent variable. It also has to have an associated \code{predictRisk} method. See \code{?predictRisk} for more details.
  }
  \item{na.action}{
How missing values should be handled. Can be one of: \code{na.fail}, \code{na.omit}, \code{na.pass}, \code{na.exclude} or a user-defined custom function. Also accepts strings of the function names. See \code{?na.action} for more details. By default it uses the na.action which is set in the global options by the respective user.
  }
  \item{tau}{
The point in time to which the RMST should be calculated. Corresponds to the \code{to} argument in the \code{adjusted_rmst} function of the \code{adjustedCurves} package. If multiple values are supplied, one curve is drawn for each of them.
  }
  \item{horizon}{
A numeric vector containing a range of values of \code{variable} for which the survival curves should be calculated or \code{NULL} (default). If \code{NULL}, the horizon is constructed as a sequence from the lowest to the highest value observed in \code{variable} with 100 equally spaced steps.
  }
  \item{custom_colors}{
An optional character vector of colors to use when there are multiple values in \code{tau}. Ignored if \code{length(tau)==1}, in which case the \code{color} argument can be used to specify the color of the single line.
  }
  \item{size}{
A single number specifying how thick the lines should be drawn.
  }
  \item{linetype}{
The linetype of the drawn lines. See documentation of \pkg{ggplot2} for more details on allowed values.
  }
  \item{alpha}{
The transparency level of the lines.
  }
  \item{color}{
The color of the curve if \code{tau} is a single value. If a numeric vector was supplied to \code{tau}, use the \code{custom_colors} argument to specify them instead.
  }
  \item{xlab}{
A character string used as the x-axis label of the plot.
  }
  \item{ylab}{
A character string used as the y-axis label of the plot.
  }
  \item{title}{
A character string used as the title of the plot.
  }
  \item{subtitle}{
A character string used as the subtitle of the plot.
  }
  \item{legend.title}{
A character string used as the legend title of the plot.
  }
  \item{legend.position}{
Where to put the legend. See \code{?theme} for more details.
  }
  \item{gg_theme}{
A \pkg{ggplot2} theme which is applied to the plot.
  }
  \item{...}{
Further arguments passed to \code{curve_cont}.
  }
}
\details{
Similar to the \code{plot_surv_at_t} and \code{plot_surv_quantiles} plots, this function produces a plot of a survival curve summary statistic as a function of a continuous variable. The summary statistic in question is the restricted mean survival time (RMST), which is the area under a survival curve up to a specific point in time \code{tau}. It can be interpreted as the mean survival time of the population up to \code{tau}.

First, a range of value-specific survival curves are estimated. The RMST is then calculated for each one using the \code{adjusted_rmst} function from the \code{adjustedCurves} package. This only works for survival curves, not for CIFs. If the area under the CIF should be used instead, the \code{plot_surv_rmtl} function can be used.

An advantage of this method over the \code{plot_surv_at_t} and \code{plot_surv_quantiles} function is, that it does take the whole survival curve into account, kind of. Although it is the area under it, it is only calculated up to \code{tau} which makes the output dependent on the choice of \code{tau}. This, again, can result in deceiving plots in some cases. Plots visualizing the entire survival curves such as \code{plot_surv_contour} and \code{plot_surv_area} might be preferable.
}
\value{
Returns a \code{ggplot2} object.
}
\author{
Robin Denz
}
\references{
Eng, K. H.; Schiller, E. & Morrell, K. On Representing the Prognostic Value of Continuous Gene Expression Biomarkers with the Restricted Mean Survival Curve. In: Oncotarget, 2015, 6, 36308-36318
}
\examples{
\dontrun{
library(riskRegression)
library(survival)
library(ggplot2)
library(adjustedCurves)
library(splines)

# using data from the survival package
data(nafld, package="survival")

# fit cox-model with age
model <- coxph(Surv(futime, status) ~ age, data=nafld1, x=TRUE)

# plot effect of age on the RMST for ages 50 to 80
plot_surv_rmst(time="futime",
               status="status",
               variable="age",
               data=nafld1,
               model=model,
               horizon=seq(50, 80, 1),
               tau=2500)

# plot RMST for multiple tau values for ages 50 to 80
plot_surv_rmst(time="futime",
               status="status",
               variable="age",
               data=nafld1,
               model=model,
               horizon=seq(50, 80, 1),
               tau=c(2000, 3000, 5000))

## showing non-linear effects

# fit cox-model with bmi modeled using B-Splines,
# adjusting for age and sex
model2 <- coxph(Surv(futime, status) ~ age + male + bs(bmi, df=5),
                data=nafld1, x=TRUE)

# plot effect of bmi on survival
plot_surv_rmst(time="futime",
               status="status",
               variable="bmi",
               data=nafld1,
               model=model2,
               tau=c(2000, 3000, 5000))
}
}
