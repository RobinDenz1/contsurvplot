\name{curve_cont}
\alias{curve_cont}

\title{
Calculate Causal or Failure Probabilities for Levels of a Continuous Variable
}
\description{
This function can be utilized to calculate survival curves or cumulative incidence functions (CIF) for specific values of a continuous covariate.
}
\usage{
curve_cont(data, variable, model, horizon,
           times, cause=1, cif=FALSE,
           na.action=options()$na.action, ...)
}
\arguments{
  \item{data}{
A \code{data.frame} containing all required variables.
  }
  \item{variable}{
A single character string specifying the continuous variable of interest, for which the survival curves should be estimated. This variable has to be contained in the \code{data.frame} that is supplied to the \code{data} argument.
  }
  \item{model}{
A model describing the time-to-event process (such as an \code{coxph} model). Needs to include \code{variable} as an independent variable. It also has to have an associated \code{predictRisk} method. See \code{?predictRisk} for more details.
  }
  \item{horizon}{
A numeric vector containing a range of values of \code{variable} for which the survival curves should be calculated.
  }
  \item{times}{
A numeric vector containing points in time at which the survival probabilities should be calculated.
  }
  \item{cause}{
The cause of interest. In standard survival data with only one event type, this should be kept at 1.
  }
  \item{cif}{
Whether to calculate the cumulative incidence (CIF) instead of the survival probability. If multiple failure types are present, the survival probability cannot be estimated in an unbiased way. This function will always return CIF estimates in that case.
  }
  \item{na.action}{
How missing values should be handled. Can be one of: \code{na.fail}, \code{na.omit}, \code{na.pass}, \code{na.exclude} or a user-defined custom function. Also accepts strings of the function names. See \code{?na.action} for more details. By default it uses the na.action which is set in the global options by the respective user.
  }
  \item{...}{
Further arguments passed to \code{predictRisk}.
  }
}
\details{
This function uses Direct Standardizaion, also known as the Corrected Group Prognosis method, G-Computation or G-Formula, to calculate the causal survival probability or CIF for values of a continuous variable. The resulting estimates can be interepreted as the average survival probability (or failure probability) that would have been observed if the value of \code{variable} had been set to a specific value for all individuals in \code{data}.

If the model supplied to \code{model} is correct, the resulting estimates will be unbiased. By including relevant confounders as independent covariates in the \code{model}, the resulting estimates are adjusted for the confounders as well. This function is used internally in all plot functions included in this R-package.

It relies on the \code{predictRisk} function from the \pkg{riskRegression} package to create the covariate and time specific estimates of the probabilities.
}
\value{
Returns a \code{data.frame} containing the columns \code{time} (the point in time), \code{surv} (the estimated survival probability) and \code{cont} (the specific value of \code{variable} used).
}
\author{
Robin Denz
}

\seealso{
\code{\link[=riskRegression]{predictRisk}}
}
\examples{
\dontrun{

}
}
