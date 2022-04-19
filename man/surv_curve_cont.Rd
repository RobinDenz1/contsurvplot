\name{surv_curve_cont}
\alias{surv_curve_cont}

\title{
Calculate Causal Survival Curves for Levels of a Continuous Variable
}
\description{
This function can be utilized to calculate survival curves for specific values of a continuous covariate.
}
\usage{
surv_curve_cont(data, variable, outcome_model, horizon,
                times, ...)
}
\arguments{
  \item{data}{
A \code{data.frame} containing all required variables.
  }
  \item{variable}{
A single character string specifying the continuous variable of interest, for which the survival curves should be estimated. This variable has to be contained in the \code{data.frame} that is supplied to the \code{data} argument.
  }
  \item{outcome_model}{
A model describing the time-to-event process (such as an \code{coxph} model). Needs to include \code{variable} as an independent variable. It also has to have an associated \code{predictRisk} method. See \code{?predictRisk} for more details.
  }
  \item{horizon}{
A numeric vector containing a range of values of \code{variable} for which the survival curves should be calculated.
  }
  \item{times}{
A numeric vector containing points in time at which the survival probabilities should be calculated.
  }
  \item{...}{
Further arguments passed to \code{predictRisk}.
  }
}
\details{
This function uses Direct Standardizaion, also known as the Corrected Group Prognosis method, G-Computation or G-Formula, to calculate the causal survival probability for values of a continuous variable. The resulting estimates can be interepreted as the average survival probability that would have been observed if the value of \code{variable} had been set to a specific value for all individuals in \code{data}.

If the model supplied to \code{outcome_model} is correct, the resulting estimates will be unbiased. By including relevant confounders as independent covariates in the \code{outcome_model}, the resulting estimates are adjusted for the confounders as well. This function is used internally in all plot functions included in this R-package.

It relies on the \code{predictRisk} function from the \pkg{riskRegression} package to create the covariate and time specific estimates of the survival probabilities.
}
\value{
Returns a \code{data.frame} containing the columns \code{time} (the point in time), \code{surv} (the estimated survival probability) and \code{cont} (the specific value of \code{variable} used).
}
\author{
Robin Denz
}

\seealso{
\code{\link{adjustedsurv}}, \code{\link{plot_rmst_curve}}
}
\examples{
\dontrun{
library(survival)

# simulate some data as example
sim_dat <- sim_confounded_surv(n=500, max_t=1.2)
sim_dat$group <- as.factor(sim_dat$group)

# estimate a cox-regression for the outcome
cox_mod <- coxph(Surv(time, event) ~ x1 + x2 + x3 + x4 + x5 + x6 + group,
                 data=sim_dat, x=TRUE)


# use it to calculate adjusted survival curves with bootstrapping
adjsurv <- adjustedsurv(data=sim_dat,
                        variable="group",
                        ev_time="time",
                        event="event",
                        method="direct",
                        outcome_model=cox_mod,
                        conf_int=FALSE,
                        bootstrap=TRUE,
                        n_boot=500)

# calculate adjusted restricted mean survival times from 0 to 1
adjrmst <- adjusted_rmst(adjsurv, from=0, to=1, conf_int=FALSE)

# calculate adjusted restricted mean survival times from 0 to 1,
# including standard errors and confidence intervals
adjrmst <- adjusted_rmst(adjsurv, from=0, to=1, conf_int=TRUE,
                         conf_level=0.95)
}
}
