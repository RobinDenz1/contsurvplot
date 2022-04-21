\name{plot_surv_contour}
\alias{plot_surv_contour}

\title{
Create a Contour Plot of the Effect of a Continuous Covariate on the Survival Probability
}
\description{
Using a previously fit time-to-event model, this function creates a contour plot with the continuous covariate on the y-axis and the time-to-event on the x-axis. The color is made in accordence with the corresponding survival probability at that point, binned into separate categories. This is very similar to the \code{plot_surv_heatmap} plot, but using a categorical representation.
}
\usage{
plot_surv_contour(time, status, variable, data, model,
                  cif=FALSE, na.action=options()$na.action,
                  horizon=NULL, fixed_t=NULL, max_t=Inf,
                  size=0.1, linetype="solid", alpha=1,
                  bins=NULL, binwidth=NULL, breaks=NULL,
                  custom_colors=NULL,
                  xlab="Time", ylab=variable,
                  title=NULL, subtitle=NULL,
                  legend.title="S(t)", legend.position="right",
                  gg_theme=ggplot2::theme_bw(),
                  panel_border=FALSE, axis_dist=0, ...)
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
A numeric vector containing points in time at which the survival probabilities should be calculated or \code{NULL} (default). If \code{NULL}, the survival probability is estimated at 100 equally spaced steps from 0 to the maximum observed event time.
  }
  \item{max_t}{
A number indicating the latest survival time which is to be plotted.
  }
  \item{size}{
The size of the individual lines plotted by the \code{geom_contour_filled} function. Usually this option can be kept at 0.1 (default).
  }
  \item{linetype}{
The linetype of the contour lines.
  }
  \item{alpha}{
The transparency level of the plot.
  }
  \item{bins}{
A single number specifying how many categories the survival probability or CIF should be divided in, or \code{NULL} (default). If \code{NULL}, the defaults in \code{geom_contour_filled} are used.
  }
  \item{binwidth}{
A single number specifying the width of the bins of the survival probability or CIF categories, or \code{NULL} (default). If \code{NULL}, the defaults in \code{geom_contour_filled} are used.
  }
  \item{breaks}{
A vector of specific breaks to create the survival probability or CIF bins, or \code{NULL} (default). If \code{NULL}, the defaults in \code{geom_contour_filled} are used.
  }
  \item{custom_colors}{
A character vector of custom colors that should be used for each contour area.
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
  \item{panel_border}{
Whether to draw a border around the heatmap or not. Is set to FALSE by default to mimic standard heatmaps.
  }
  \item{axis_dist}{
The distance of the axis ticks to the colored heatmap. Is set to 0 by default to mimic standard heatmaps.
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
