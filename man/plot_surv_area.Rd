\name{plot_surv_area}
\alias{plot_surv_area}

\title{
Plot a Survival Area Plot for the Effect of a Continuous Variable on a Time-To-Event Outcome
}
\description{
Using a previously fit time-to-event model, this function plots a survival curve or CIF area plot. Instead of plotting value-specific curves, the probability of interest is represented as an area as a function of time, where the color changes according to the continuous variable.
}
\usage{
plot_surv_area(time, status, variable, data, model,
               cif=FALSE, na.action=options()$na.action,
               horizon=NULL, fixed_t=NULL, max_t=Inf,
               start_color="blue", end_color="red", alpha=1,
               discrete=FALSE, bins=ifelse(discrete, 10, 40),
               sep_lines=FALSE, sep_color="black",
               sep_size=0.1, sep_linetype="solid",
               sep_alpha=alpha, xlab="Time",
               ylab="Survival Probability", title=NULL,
               subtitle=NULL, legend.title=variable,
               legend.position="right",
               gg_theme=ggplot2::theme_bw(),
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
  \item{data}{
A \code{data.frame} containing all required variables.
  }
  \item{model}{
A model describing the time-to-event process (such as an \code{coxph} model). Needs to include \code{variable} as an independent variable. It also has to have an associated \code{predictRisk} method. See \code{?predictRisk} for more details.
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
A numeric vector containing points in time at which the survival probabilities should be calculated or \code{NULL} (default). If \code{NULL}, the survival probability is estimated at every point in time at which an event occured.
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
The transparency level of the main plot.
  }
  \item{discrete}{
Whether to plot the area as a single continuously shaded area (default) or as multiple discrete blocks. The blocks correspond to ranges (such as 5-10). This only really changes the style of the legend and decreases the default value of \code{bins} to make the discrete blocks visable.
  }
  \item{bins}{
The number of bins the survival area should be divided into. In a nutshell, this function simply estimates a lot of covariate specific curves and colors the area between them on a graded scale. If many bins are used, this gives the appearance of a single shaded area, changing continuously. By using only a few bins however, the specific covariate values might be easier to read off the plot. See also the \code{discrete} argument.
  }
  \item{sep_lines}{
Whether to draw lines between the individual area segments or not. When \code{discrete=TRUE} is used, this might be a good option to help visually separate the segments.
  }
  \item{sep_color}{
The color of the separator lines. Ignored if \code{sep_lines=FALSE}.
  }
  \item{sep_size}{
The size of the separator lines. Ignored if \code{sep_lines=FALSE}.
  }
  \item{sep_linetype}{
The linetype of the separator lines. Ignored if \code{sep_lines=FALSE}.
  }
  \item{sep_alpha}{
The transparency level of the separator lines. Ignored if \code{sep_lines=FALSE}.
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
This function is very similar to a contour plot (see \code{plot_surv_contour}), but with the probability of interest on the y-axis and the color representing the values of the continuous variable of interest. The main advantage of this type of plot is, that it has the same structure as a usual Kaplan-Meier plot. The only difference is that instead of single curves stratified by some variable, the curves are plotted as an area instead.

This is achieved by estimating value-specific curves over the whole range of the continuous covariate using direct standardization and a previously fitted time-to-event model (see \code{curve_cont}). The area between those curves is then simply filled in (using the \code{geom_stepribbon} function from the \code{pammtools} package). By using a big number of individual curves, the appearance of a continuously shaded are is created. This is done by default when using the argument \code{discrete=TRUE}. By using \code{discrete=FALSE}, only a small number of value-specific curves are estimated, resulting in only a few discrete bins. When using only a few bins, the output is even closer to a contour plot.

The major downside of this plotting method is that it only works if the effect of the continuous covariate was modelled as a linear effect or is montonically increasing or decreasing. Curved relationships between the variable and the survival time cannot be represented using this function. However, the \code{plot_surv_contour} and \code{plot_surv_heatmap} functions may be used instead in this case.
}
\value{
Returns a \code{ggplot2} object.
}
\author{
Robin Denz
}
\examples{
\dontrun{
library(riskRegression)
library(survival)
library(ggplot2)
library(pammtools)

# using data from the survival package
data(nafld, package="survival")

# fit cox-model with age
model <- coxph(Surv(futime, status) ~ age, data=nafld1, x=TRUE)

# plot effect of age on survival using defaults
plot_surv_area(time="futime",
               status="status",
               variable="age",
               data=nafld1,
               model=model)

# plot it only for 60 to 80 year old people
plot_surv_area(time="futime",
               status="status",
               variable="age",
               data=nafld1,
               model=model,
               horizon=seq(60, 80, 0.5))

# plot it only for 60 to 80 year old people,
# and also use a black and white color scale
plot_surv_area(time="futime",
               status="status",
               variable="age",
               data=nafld1,
               model=model,
               horizon=seq(60, 80, 0.5),
               start_color="grey",
               end_color="black")

# plot it only for 60 to 60 year old people, using discrete bins
plot_surv_area(time="futime",
               status="status",
               variable="age",
               data=nafld1,
               model=model,
               horizon=seq(60, 80, 5),
               discrete=TRUE)
}
}
