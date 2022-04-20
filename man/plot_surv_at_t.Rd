\name{plot_surv_at_t}
\alias{plot_surv_at_t}

\title{
Plot the Survival Probability at a Fixed Point in Time as a Function of a Continuous Variable
}
\description{
Using a previously fit time-to-event model, this function plots the survival probability at one or multiple user defined points at time as a function of a continuous variable.
}
\usage{
plot_surv_at_t(time, status, variable, data, model,
               cif=FALSE, na.action=options()$na.action,
               t, horizon=NULL,
               size=1, linetype="solid", alpha=1,
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
  \item{cif}{
Whether to plot the cumulative incidence (CIF) instead of the survival probability. If multiple failure types are present, the survival probability cannot be estimated in an unbiased way. This function will always return CIF estimates in that case.
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
