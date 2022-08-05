\name{plot_surv_lines}
\alias{plot_surv_lines}

\title{
Plot Individual Survival Curves or CIFs for Specific Values of a Continuous Covariate
}
\description{
Using a previously fit time-to-event model, this function plots survival curves or CIFs that would have been observed if every individual in the dataset had been set to specific values of a continuous covariate.
}
\usage{
plot_surv_lines(time, status, variable, group=NULL,
                data, model, cif=FALSE,
                na.action=options()$na.action,
                horizon=NULL, fixed_t=NULL, max_t=Inf,
                discrete=TRUE, custom_colors=NULL,
                start_color="blue", end_color="red",
                size=1, linetype="solid", alpha=1,
                xlab="Time", ylab="Survival Probability",
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
  \item{model}{
A model describing the time-to-event process (such as an \code{coxph} model). Needs to include \code{variable} as an independent variable. It also has to have an associated \code{\link[=riskRegression]{predictRisk}} method. See \code{?predictRisk} for more details.
  }
  \item{cif}{
Whether to plot the cumulative incidence (CIF) instead of the survival probability. If multiple failure types are present, the survival probability cannot be estimated in an unbiased way. This function will always return CIF estimates in that case.
  }
  \item{na.action}{
How missing values should be handled. Can be one of: \code{na.fail}, \code{na.omit}, \code{na.pass}, \code{na.exclude} or a user-defined custom function. Also accepts strings of the function names. See \code{?na.action} for more details. By default it uses the na.action which is set in the global options by the respective user.
  }
  \item{horizon}{
A numeric vector containing a range of values of \code{variable} for which the survival curves should be calculated or \code{NULL} (default). If \code{NULL}, the horizon is constructed as a sequence from the lowest to the highest value observed in \code{variable} with 12 equally spaced steps.
  }
  \item{fixed_t}{
A numeric vector containing points in time at which the survival probabilities should be calculated or \code{NULL} (default). If \code{NULL}, the survival probability is estimated at every point in time at which an event occured.
  }
  \item{max_t}{
A number indicating the latest survival time which is to be plotted.
  }
  \item{discrete}{
Whether to use a continuous color scale or a discrete one (default). If \code{FALSE}, the default \pkg{ggplot2} colors are used.
  }
  \item{custom_colors}{
An optional character vector of colors to use when \code{discrete=FALSE}. Ignored if \code{discrete=TRUE}, in which case the color gradient can be defined using the \code{start_color} and \code{end_color} arguments.
  }
  \item{start_color}{
The color used for the lowest value in \code{horizon}. This and the \code{end_color} argument can be used to specify custom continuous color scales used in the plot. For example, if a black and white plot is desired, the user can set \code{start_color="white"} and \code{end_color="black"}. See \code{?scale_color_gradient} for more information.
  }
  \item{end_color}{
The color used for the highest value in \code{horizon}. See argument \code{start_color}.
  }
  \item{size}{
A single number specifying the size of the drawn curves.
  }
  \item{linetype}{
A single character string specifyinf the linetype of the curves.
  }
  \item{alpha}{
The transparency level of the plot.
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
  \item{...}{
Further arguments passed to \code{\link{curve_cont}}.
  }
}
\details{
A simple plot of multiple covariate-specific survival curves. Internally, it uses the \code{\link{curve_cont}} function to calculate the survival curves.
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

# plot effect of age on survival using defaults
plot_surv_lines(time="futime",
                status="status",
                variable="age",
                data=nafld1,
                model=model)

# plot it only for some specific user-defined values
plot_surv_lines(time="futime",
                status="status",
                variable="age",
                data=nafld1,
                model=model,
                horizon=c(40, 52, 63, 81))

## showing non-linear effects

# fit cox-model with bmi modelled using B-Splines,
# adjusting for age and sex
model2 <- coxph(Surv(futime, status) ~ age + male + bs(bmi, df=3),
                data=nafld1, x=TRUE)

# plot effect of bmi on survival
plot_surv_heatmap(time="futime",
                status="status",
                variable="bmi",
                data=nafld1,
                model=model2)
}
