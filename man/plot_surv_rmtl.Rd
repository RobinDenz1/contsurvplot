\name{plot_surv_rmtl}
\alias{plot_surv_rmtl}

\title{
Plot the Effect of a Continuous Variable on the Restricted Mean Time Lost
}
\description{
Using a previously fit time-to-event model, this function plots the restricted mean time lost (RMTL) as a function of a continuous variable. Unlike the \code{\link{plot_surv_rmst}} function, this function can deal with multiple event types in the \code{status} variable.
}
\usage{
plot_surv_rmtl(time, status, variable, group=NULL, cause=1,
               data, model, conf_int=FALSE, conf_level=0.95,
               n_boot=300, na.action=options()$na.action,
               tau, horizon=NULL, custom_colors=NULL,
               size=1, linetype="solid", alpha=1, color="black",
               xlab=variable, ylab="Restricted Mean Time Lost",
               title=NULL, subtitle=NULL,
               legend.title="tau", legend.position="right",
               gg_theme=ggplot2::theme_bw(),
               facet_args=list(), ci_alpha=0.4, ...)
}
\arguments{
  \item{time}{
A single character string specifying the time-to-event variable. Needs to be a valid column name of a variable in \code{data}.
  }
  \item{status}{
A single character string specifying the status variable, indicating if a person has experienced an event or not. Needs to be a valid column name of a variable in \code{data}.
  }
  \item{variable}{
A single character string specifying the continuous variable of interest, for which the CIFs should be estimated. This variable has to be contained in the \code{data.frame} that is supplied to the \code{data} argument.
  }
  \item{group}{
An optional single character string specifying a factor variable in \code{data}. When used, the plot is created conditional on this factor variable, meaning that a facetted plot is produced with one facet for each level of the factor variable. See \code{\link{curve_cont}} for a detailed description of the estimation strategy. Set to \code{NULL} (default) to use no grouping variable.
  }
  \item{cause}{
The cause of interest. In standard survival data with only one event type, this should be kept at 1. For data with multiple failure types, this argument should be specified.
  }
  \item{data}{
A \code{data.frame} containing all required variables.
  }
  \item{model}{
A model describing the time-to-event process (such as an \code{coxph} model). Needs to include \code{variable} as an independent variable. It also has to have an associated \code{\link[riskRegression]{predictRisk}} method. See \code{?predictRisk} for more details.
  }
  \item{conf_int}{
Whether to plot point-wise bootstrap confidence intervals or not.
  }
  \item{conf_level}{
A number specifying the confidence level of the bootstrap confidence intervals. Ignored if \code{conf_int=FALSE}.
  }
  \item{n_boot}{
A single integer specifying how many bootstrap repetitions should be performed. Ignored if  \code{conf_int=FALSE}.
  }
  \item{na.action}{
How missing values should be handled. Can be one of: \code{na.fail}, \code{na.omit}, \code{na.pass}, \code{na.exclude} or a user-defined custom function. Also accepts strings of the function names. See \code{?na.action} for more details. By default it uses the na.action which is set in the global options by the respective user.
  }
  \item{tau}{
The point in time to which the RMTL should be calculated. Can be a vector of numbers. If multiple values are supplied, one curve is drawn for each of them.
  }
  \item{horizon}{
A numeric vector containing a range of values of \code{variable} for which the CIFs should be calculated or \code{NULL} (default). If \code{NULL}, the horizon is constructed as a sequence from the lowest to the highest value observed in \code{variable} with 100 equally spaced steps.
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
  \item{facet_args}{
A named list of arguments that are passed to the \code{\link[ggplot2]{facet_wrap}} function call when creating a plot separated by groups. Ignored if \code{group=NULL}. Any argument except the \code{facets} argument of the \code{\link[ggplot2]{facet_wrap}} function can be used. For example, if the user wants to allow free y-scales, this argument could be set to \code{list(scales="free_y")}.
  }
  \item{ci_alpha}{
A single number defining the transparency level of the confidence interval bands.
  }
  \item{...}{
Further arguments passed to \code{\link{curve_cont}}.
  }
}
\details{
This function is essentially equal to the \code{\link{plot_surv_rmtl}} function. The only difference is that instead of using the restricted mean survival time (RMST), which is the area under the survival curve up to a specific point in time \code{tau}, this function uses the restricted mean time lost (RMTL), which is the area under the (cause-specific) CIF up to a specific point in time \code{tau}. It can be interpreted as the mean time it takes an individual to succumb to the event of interest before \code{tau}.

The reason this is split into two functions is, that the RMTL can be calculated when mutually exclusive competing events are present, while the RMST cannot. The basic pros and cons of the \code{\link{plot_surv_rmst}} function still apply here, however. For more information we suggest consulting the documentation page of the \code{\link{plot_surv_rmst}} function.
}
\value{
Returns a \code{ggplot2} object.
}
\author{
Robin Denz
}
\references{
Eng, K. H.; Schiller, E. & Morrell, K. On Representing the Prognostic Value of Continuous Gene Expression Biomarkers with the Restricted Mean Survival Curve. In: Oncotarget, 2015, 6, 36308-36318

Robin Denz, Nina Timmesfeld (2023). "Visualizing the (Causal) Effect of a Continuous Variable on a Time-To-Event Outcome". In: Epidemiology 34.5
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

# plot effect of age on the RMST for ages 50 to 80
plot_surv_rmtl(time="futime",
               status="status",
               variable="age",
               data=nafld1,
               model=model,
               horizon=seq(50, 80, 1),
               tau=2500)

# plot RMST for multiple tau values for ages 50 to 80
plot_surv_rmtl(time="futime",
               status="status",
               variable="age",
               data=nafld1,
               model=model,
               horizon=seq(50, 80, 1),
               tau=c(2000, 3000, 5000))

## showing non-linear effects

# fit cox-model with bmi modeled using B-Splines,
# adjusting for age and sex
model2 <- coxph(Surv(futime, status) ~ age + male + bs(bmi, df=3),
                data=nafld1, x=TRUE)

# plot effect of bmi on survival
plot_surv_rmtl(time="futime",
               status="status",
               variable="bmi",
               data=nafld1,
               model=model2,
               tau=c(2000, 3000, 5000))
}
