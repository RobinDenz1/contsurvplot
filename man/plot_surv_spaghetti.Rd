\name{plot_surv_spaghetti}
\alias{plot_surv_spaghetti}

\title{
Plot Individual Survival Curves for Specific Values of a Continuous Covariate
}
\description{
Using a previously fit time-to-event model, this function plots survival curves that would have been observed if every individual in the dataset had been set to specific values of a continuous covariate.
}
\usage{
plot_surv_spaghetti(time, status, variable, data, model,
                    horizon=NULL, fixed_t=NULL, max_t=Inf,
                    start_color="blue", end_color="red",
                    size=0.5, linetype="solid", alpha=1,
                    xlab="Time", ylab="Survival Probability",
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
  \item{size}{

  }
  \item{linetype}{

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
A simple plot of multiple covariate-specific survival curves. Internally, it uses the \code{surv_curve_cont} function to calculate the survival curves.
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
