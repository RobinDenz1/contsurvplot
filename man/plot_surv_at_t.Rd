\name{plot_surv_at_t}
\alias{plot_surv_at_t}

\title{
Plot the Survival Probability or CIF at a Fixed Point in Time as a Function of a Continuous Variable
}
\description{
Using a previously fit time-to-event model, this function plots the survival probability or CIF at one or multiple user defined points at time as a function of a continuous variable.
}
\usage{
plot_surv_at_t(time, status, variable, group=NULL,
               data, model, cif=FALSE, conf_int=FALSE,
               conf_level=0.95, n_boot=300,
               na.action=options()$na.action, t, horizon=NULL,
               size=1, linetype="solid", alpha=1,
               xlab=variable, ylab="Survival Probability at t",
               title=NULL, subtitle=NULL,
               legend.title="t", legend.position="right",
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
A single character string specifying the continuous variable of interest, for which the survival curves should be estimated. This variable has to be contained in the \code{data.frame} that is supplied to the \code{data} argument.
  }
  \item{group}{
An optional single character string specifying a factor variable in \code{data}. When used, the plot is created conditional on this factor variable, meaning that a facetted plot is produced with one facet for each level of the factor variable. See \code{\link{curve_cont}} for a detailed description of the estimation strategy. Set to \code{NULL} (default) to use no grouping variable.
  }
  \item{data}{
A \code{data.frame} containing all required variables.
  }
  \item{model}{
A model describing the time-to-event process (such as an \code{coxph} model). Needs to include \code{variable} as an independent variable. It also has to have an associated \code{\link[=riskRegression]{predictRisk}} method. See \code{?predictRisk} for more details.
  }
  \item{cif}{
Whether to plot the cumulative incidence (CIF) instead of the survival probability. If multiple failure types are present, the survival probability cannot be estimated in an unbiased way. This function will always return CIF estimates in that case.
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
  \item{t}{
The point in time at which the survival probability should be calculated. For example, by setting this parameter to 10, this function will display the survival probability at t = 10 over values of \code{variable}. If multiple values are supplied, one curve is drawn for each of them.
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
A named list of arguments that are passed to the \code{\link{facet_wrap}} function call when creating a plot separated by groups. Ignored if \code{group=NULL}. Any argument except the \code{facets} argument of the \code{\link{facet_wrap}} function can be used. For example, if the user wants to allow free y-scales, this argument could be set to \code{list(scales="free_y")}.
  }
  \item{ci_alpha}{
A single number defining the transparency level of the confidence interval bands.
  }
  \item{...}{
Further arguments passed to \code{\link{curve_cont}}.
  }
}
\details{
By picking a single value of time, we can effectively reduce the dimensions of the plot by one, resulting in a simple curve plot of the survival probability or CIF at the specific point in time as it changes over values of the continuous covariate. By supplying a vector of times to the \code{t} argument, multiple such curves can be produced in the same plot.

This can be a very effective plotting strategy if there are specific points in time of interest already (even better if those were pre-defined). It is however a poor strategy if a representation of the effect on the survival over time is desired. An empirical example of this strategy using the 5 year risk is given in Shen et al. (2017).

As all plot functions in this package, this plot relies on the \code{\link{curve_cont}} function to calculate the needed probability estimates.
}
\value{
Returns a \code{ggplot2} object.
}
\author{
Robin Denz
}
\references{
Shen, Y.-M.; Le, L. D.; Wilson, R. & Mansmann, U. Graphical Presentation of Patient-Treatment Interaction Elucidated by Continuous Biomarkers: Current Practice and Scope for Improvement Methods of Information in Medicine, 2017, 56, 13-27

Robin Denz, Nina Timmesfeld (2023). "Visualizing the (Causal) Effect of a Continuous Variable on a Time-To-Event Outcome". In: Epidemiology 34.5
}
\examples{
library(contsurvplot)
library(riskRegression)
library(survival)
library(ggplot2)

# using data from the survival package
data(nafld, package="survival")

# take a random sample to keep example fast
set.seed(42)
nafld1 <- nafld1[sample(nrow(nafld1), 150), ]

# fit cox-model with age
model <- coxph(Surv(futime, status) ~ age, data=nafld1, x=TRUE)

# plot effect of age on survival at t=2000
plot_surv_at_t(time="futime",
               status="status",
               variable="age",
               data=nafld1,
               model=model,
               t=2000)

# plot it for arbitrary multiple values of t
plot_surv_at_t(time="futime",
               status="status",
               variable="age",
               data=nafld1,
               model=model,
               t=c(1000, 2000, 3200, 5643))
}
