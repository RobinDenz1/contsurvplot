\name{plot_surv_animated}
\alias{plot_surv_animated}

\title{
Create an Animated Plot of Survival Curves of CIFs as a Function of a Continuous Variable
}
\description{
Using a previously fit time-to-event model, this function plots a single covariate-specific survival curve or CIF. Using either the \pkg{plotly} or the \pkg{gganimate} package, an animated plot is created that is either simply showing how the curve evolves over values of the continuous covariate, or includes a slide to let the user interact with it.
}
\usage{
plot_surv_animated(time, status, variable, data, model,
                   cif=FALSE, na.action=options()$na.action,
                   horizon=NULL, fixed_t=NULL, max_t=Inf,
                   slider=TRUE, size=1, color="black",
                   linetype="solid", alpha=1,
                   xlab="Time", ylab="Survival Probability",
                   title=NULL, subtitle=NULL,
                   gg_theme=ggplot2::theme_bw(), ...)
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
  \item{slider}{
Whether to include a slider that controls the value of the continuous covariate or not. If not, the plot simply cycles through the values in \code{horizon} at a constant speed.
  }
  \item{size}{
A single number specifying the size of the drawn curve.
  }
  \item{color}{
A single character string specifying the color of the curve.
  }
  \item{linetype}{
A single character string specifying the linetype of the curve.
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
A character string used as the title of the plot. When \code{slider=FALSE} is used, the title is also used in a dynamic way to show which value the continuous variable currently has. The user-defined title is still shown, but the covariate level is also appended to it in that case.
  }
  \item{subtitle}{
A character string used as the subtitle of the plot.
  }
  \item{gg_theme}{
A \pkg{ggplot2} theme which is applied to the plot.
  }
  \item{...}{
Further arguments passed to \code{predictRisk}.
  }
}
\details{

}
\value{
Returns a \code{ggplot2} object.
}
\author{
Robin Denz
}
\examples{
\dontrun{

}
}