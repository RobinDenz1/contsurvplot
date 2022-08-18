\name{plot_surv_heatmap}
\alias{plot_surv_heatmap}

\title{
Plot a Heatmap of the Effect of a Continuous Covariate on a Time-To-Event Outcome
}
\description{
Using a previously fit time-to-event model, this function plots a heatmap with the continuous covariate on the y-axis and the time-to-event on the x-axis. The color is made in accordance with the corresponding survival probability or CIF at that point.
}
\usage{
plot_surv_heatmap(time, status, variable, group=NULL,
                  data, model, cif=FALSE,
                  na.action=options()$na.action,
                  horizon=NULL, fixed_t=NULL, max_t=Inf,
                  start_color=NULL, end_color=NULL,
                  alpha=1, xlab="Time", ylab=variable,
                  title=NULL, subtitle=NULL,
                  legend.title="S(t)", legend.position="right",
                  gg_theme=ggplot2::theme_bw(),
                  facet_args=list(), panel_border=FALSE,
                  axis_dist=0, interpolate=TRUE,
                  contour_lines=FALSE, contour_color="white",
                  contour_size=0.3, contour_linetype="dashed",
                  ...)
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
A numeric vector containing a range of values of \code{variable} for which the survival curves should be calculated or \code{NULL} (default). If \code{NULL}, the horizon is constructed as a sequence from the lowest to the highest value observed in \code{variable} with 40 equally spaced steps.
  }
  \item{fixed_t}{
A numeric vector containing points in time at which the survival probabilities should be calculated or \code{NULL} (default). If \code{NULL}, the survival probability is estimated at 100 equally spaced steps from 0 to the maximum observed event time.
  }
  \item{max_t}{
A number indicating the latest survival time which is to be plotted.
  }
  \item{start_color}{
The color used for the lowest value in \code{horizon}. This and the \code{end_color} argument can be used to specify custom continuous color scales used in the plot. For example, if a black and white plot is desired, the user can set \code{start_color="white"} and \code{end_color="black"}. See \code{?scale_color_gradient} for more information.
  }
  \item{end_color}{
The color used for the highest value in \code{horizon}. See argument \code{start_color}.
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
  \item{panel_border}{
Whether to draw a border around the heatmap or not. Is set to FALSE by default to mimic standard heatmaps.
  }
  \item{axis_dist}{
The distance of the axis ticks to the colored heatmap. Is set to 0 by default to mimic standard heatmaps.
  }
  \item{interpolate}{
Whether to linearly interpolate the colors or not. Set to \code{TRUE} by default, which results in a smooth surface being plotted. Corresponds to the \code{interpolate} argument in the \code{geom_raster} function, which is used internally.
  }
  \item{contour_lines}{
Whether to add some contour lines to the heatmap. To get a proper contour plot, use the \code{\link{plot_surv_contour}} function instead.
  }
  \item{contour_color}{
The color of the contour lines. Defaults to \code{"white"}. Ignored if \code{contour_lines=FALSE}.
  }
  \item{contour_size}{
The size of the contour lines. Ignored if \code{contour_lines=FALSE}.
  }
  \item{contour_linetype}{
The linetype of the contour lines. Defaults to \code{"dashed"}. Ignored if \code{contour_lines=FALSE}.
  }
  \item{...}{
Further arguments passed to \code{\link{curve_cont}}.
  }
}
\details{
Heatmaps are a great tool to visualize a three dimensional surface in a two-dimensional plot. A continuous color scale is used to represent the probability of interest. Although this is fine theoretically, it is often hard to read specific information off these plots. Contour lines can be added to the plot in order to make this easier by using \code{contour_lines=TRUE} in the function call.

In most cases, however, it is probably better to use a proper contour plot instead, which can be produced using the \code{\link{plot_surv_contour}} function. This is mostly a matter of taste, which is why both types of plots are included in this package. Another alternative is the to use the \code{\link{plot_surv_tiles}} function, which is basically a discretized version of a survival heatmap.

The main advantage of the heatmap and the contour plots is that they can visualize the effect of a continuous covariate regardless of how it was modeled. Non-linear relationships can be visualized just as well as linear ones. The major downside is, that the structure of the plot is not the same as that of a standard Kaplan-Meier plot. An alternative that is closer to the standard plot can be created using the \code{\link{plot_surv_area}} function.
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
plot_surv_heatmap(time="futime",
                  status="status",
                  variable="age",
                  data=nafld1,
                  model=model)

# plot it only for 60 to 80 year old people
plot_surv_heatmap(time="futime",
                  status="status",
                  variable="age",
                  data=nafld1,
                  model=model,
                  horizon=seq(60, 80, 0.5))

## showing non-linear effects

# fit cox-model with bmi modelled using B-Splines,
# adjusting for age and sex
model2 <- coxph(Surv(futime, status) ~ age + male + bs(bmi, df=3),
                data=nafld1, x=TRUE)

# plot effect of bmi on survival using defaults
plot_surv_heatmap(time="futime",
                  status="status",
                  variable="bmi",
                  data=nafld1,
                  model=model2)

# plot effect of bmi on survival with contour lines
plot_surv_heatmap(time="futime",
                  status="status",
                  variable="bmi",
                  data=nafld1,
                  model=model2,
                  contour_lines=TRUE)
}
