\name{plot_surv_quantiles}
\alias{plot_surv_quantiles}

\title{
Plot Survival Time Quantiles as a Function of a Continuous Variable
}
\description{
Using a previously fit time-to-event model, this function plots one or multiple survival time quantiles (such as the median survival time) as a function of a continuous variable of interest.
}
\usage{
plot_surv_quantiles(time, status, variable, group=NULL,
                    data, model, na.action=options()$na.action,
                    p=0.5, horizon=NULL,
                    size=1, linetype="solid", alpha=1,
                    custom_colors=NULL, single_color=NULL,
                    xlab=variable, ylab="Survival Time Quantile",
                    title=NULL, subtitle=NULL,
                    legend.title=variable, legend.position="right",
                    gg_theme=ggplot2::theme_bw(),
                    facet_args=list(), ...)
}
\arguments{
  \item{time}{
A single character string specifying the time-to-event variable. Needs to be a valid column name of a numeric variable in \code{data}.
  }
  \item{status}{
A single character string specifying the status variable, indicating if a person has experienced an event or not. Needs to be a valid column name of a numeric or logical variable in \code{data}.
  }
  \item{variable}{
A single character string specifying the continuous variable of interest, for which the survival curves should be estimated. This variable has to be contained in the \code{data.frame} that is supplied to the \code{data} argument.
  }
  \item{group}{
An optional single character string specifying a factor variable in \code{data}. When used, the plot is created conditional on this factor variable, meaning that a facetted plot is produced with one facet for each level of the factor variable. See \code{\link{curve_cont}} for a detailed description of the estimation strategy. Set to \code{NULL} (default) to use no grouping variable.
  }
  \item{data}{
A \code{data.frame} containing all required variables.
  }
  \item{p}{
A numeric vector containing the survival time quantiles of interest. For example, if the user is interested in plotting only the median survival time \code{p=0.5} should be used. When multiple values are supplied, one curve is drawn for each quantile.
  }
  \item{model}{
A model describing the time-to-event process (such as an \code{coxph} model). Needs to include \code{variable} as an independent variable. It also has to have an associated \code{\link[riskRegression]{predictRisk}} method. See \code{?predictRisk} for more details.
  }
  \item{na.action}{
How missing values should be handled. Can be one of: \code{na.fail}, \code{na.omit}, \code{na.pass}, \code{na.exclude} or a user-defined custom function. Also accepts strings of the function names. See \code{?na.action} for more details. By default it uses the na.action which is set in the global options by the respective user.
  }
  \item{horizon}{
A numeric vector containing a range of values of \code{variable} for which the survival curves should be calculated or \code{NULL} (default). If \code{NULL}, the horizon is constructed as a sequence from the lowest to the highest value observed in \code{variable} with 100 equally spaced steps.
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
  \item{custom_colors}{
An optional character vector specifying the colors that should be used when multiple quantiles were supplied to the \code{p} argument. To set the whole plot or a single curve to one color only, use the \code{single_color} argument instead.
  }
  \item{single_color}{
A single character string specifying the color of all drawn lines.
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
  \item{facet_args}{
A named list of arguments that are passed to the \code{\link[ggplot2]{facet_wrap}} function call when creating a plot separated by groups. Ignored if \code{group=NULL}. Any argument except the \code{facets} argument of the \code{\link[ggplot2]{facet_wrap}} function can be used. For example, if the user wants to allow free y-scales, this argument could be set to \code{list(scales="free_y")}.
  }
  \item{...}{
Further arguments passed to \code{\link{curve_cont}}.
  }
}
\details{
Survival Time Quantiles are a single value summarizing the entire survival curve. For example, the most prominently used survival time quantile is the median survival time, which can be interpreted as the time at which half of the people in the sample are expected to have experienced the event of interest. This plot shows one or more of these quantiles as a function of a continuous variable of interest.

To calculate the survival time quantiles, it first calls the \code{curve_cont} function to get estimates of the value-specific survival curves. Afterwards, it uses step function interpolation to read off the survival time quantile from the estimates.

Although this is a simple way to plot the effect of a continuous covariate on the survival, it can give a misleading visualization of the relationship in some situations. Plots that do not use summary statistics, such as the \code{\link{plot_surv_contour}} and \code{\link{plot_surv_area}} plots, may be preferable.
}
\value{
Returns a \code{ggplot2} object.
}
\author{
Robin Denz
}
\examples{
library(contsurvplot)
library(riskRegression)
library(survival)
library(ggplot2)
library(splines)

# using data from the survival package
data(nafld, package="survival")

# take a random sample to keep example fast
set.seed(42)
nafld1 <- nafld1[sample(nrow(nafld1), 150), ]

# fit cox-model with age
model <- coxph(Surv(futime, status) ~ age, data=nafld1, x=TRUE)

# plot effect of age on the median survival time
plot_surv_quantiles(time="futime",
                    status="status",
                    variable="age",
                    data=nafld1,
                    model=model)

# plot multiple survival time quantiles
plot_surv_quantiles(time="futime",
                    status="status",
                    variable="age",
                    data=nafld1,
                    model=model,
                    p=c(0.1, 0.25, 0.5, 0.75, 0.9))

## showing non-linear effects

# fit cox-model with bmi modelled using B-Splines,
# adjusting for age and sex
model2 <- coxph(Surv(futime, status) ~ age + male + bs(bmi, df=3),
                data=nafld1, x=TRUE)

# plot effect of bmi on survival
plot_surv_quantiles(time="futime",
                    status="status",
                    variable="bmi",
                    data=nafld1,
                    model=model2,
                    p=c(0.1, 0.25, 0.5, 0.75, 0.9))
}
