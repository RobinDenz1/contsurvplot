\name{plot_surv_heatmap}
\alias{plot_surv_heatmap}

\title{
Plot a Heatmap of the Effect of a Continuous Covariate on the Survival Probability
}
\description{
Using a previously fit time-to-event model, this function plots a heatmap with the continuous covariate on the y-axis and the time-to-event on the x-axis. The color is made in accordence with the corresponding survival probability at that point.
}
\usage{
plot_surv_heatmap(time, status, variable, data, model,
                  horizon=NULL, fixed_t=NULL, max_t=Inf,
                  start_color=NULL, end_color=NULL,
                  size=0.1, alpha=1, xlab="Time", ylab=variable,
                  title=NULL, subtitle=NULL,
                  legend.title="S(t)", legend.position="right",
                  gg_theme=ggplot2::theme_bw(),
                  panel_border=FALSE, axis_dist=0, ...)
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
  \item{horizon}{
A numeric vector containing a range of values of \code{variable} for which the survival curves should be calculated.
  }
  \item{fixed_t}{
A numeric vector containing points in time at which the survival probabilities should be calculated.
  }
  \item{max_t}{

  }
  \item{start_color}{

  }
  \item{end_color}{

  }
  \item{alpha}{

  }
  \item{xlab}{

  }
  \item{ylab}{

  }
  \item{title}{

  }
  \item{subtitle}{

  }
  \item{legend.title}{

  }
  \item{legend.position}{

  }
  \item{gg_theme}{

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
