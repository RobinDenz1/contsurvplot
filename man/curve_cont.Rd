\name{curve_cont}
\alias{curve_cont}

\title{
Estimate Counterfactual Survival or Failure Probabilities for Levels of a Continuous Variable
}
\description{
This function can be utilized to estimate counterfactual survival curves or cumulative incidence functions (CIF) for specific values of a continuous covariate.
}
\usage{
curve_cont(data, variable, model, horizon,
           times, group=NULL, cause=1, cif=FALSE,
           contrast="none", reference="km", ref_value=NULL,
           event_time=NULL, event_status=NULL,
           conf_int=FALSE, conf_level=0.95,
           n_boot=300, n_cores=1,
           na.action=options()$na.action,
           return_boot=FALSE, ...)
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
  \item{group}{
An optional single character string specifying a factor variable in \code{data}. When used, the regression standardization is performed conditional on this factor variable, meaning that one estimate is returned for each level of the factor variable. See details for a better description. Set to \code{NULL} (default) to use no grouping variable.
  }
  \item{cause}{
The cause of interest. In standard survival data with only one event type, this should be kept at 1. For data with multiple failure types, this argument should be specified. In addition, the \code{cif} argument should be set to \code{TRUE} in those cases.
  }
  \item{cif}{
Whether to calculate the cumulative incidence (CIF) instead of the survival probability. If multiple failure types are present, the survival probability cannot be estimated in an unbiased way. In those cases, this argument should always be set to \code{TRUE}.
  }
  \item{contrast}{
Defines what kind of estimate should be returned. Can be either \code{"none"} (default), \code{"diff"} or \code{"ratio"}. When \code{"none"} is used, it simply returns the counterfactual survival probabilities (or CIF if \code{cif=TRUE}) without any further calculations. If \code{"diff"} or \code{"ratio"} is used instead, the difference or ratio to some reference value will be returned. See argument below.
  }
  \item{reference}{
Defines what kind of reference value to use when estimating causal contrasts. Only used if \code{contrast!="none"}, ignored otherwise. Can be either \code{"km"} (using standard Kaplan-Meier estimates as reference) or \code{"value"} (using the g-computation estimates of a specific value of the \code{variable} as reference). To specify which value to use when using \code{reference="value"}, the \code{ref_value} argument should be used. See details for more information.
  }
  \item{ref_value}{
A single number corresponding to the reference value used when estimating causal contrasts using \code{reference="value"}. See details.
  }
  \item{event_time}{
A single character string specifying the time until the occurence of the event of interest or \code{NULL} (default). Only used when estimating causal contrasts with \code{reference="km"}, ignored otherwise. If specified, this variable has to be contained in the \code{data.frame} that is supplied to the \code{data} argument.
  }
  \item{event_status}{
A single character string specifying the status of the event of interest or \code{NULL} (default). Only used when estimating causal contrasts with \code{reference="km"}, ignored otherwise. If specified, this variable has to be contained in the \code{data.frame} that is supplied to the \code{data} argument.
  }
  \item{conf_int}{
Whether to calculate point-wise confidence intervals or not. If \code{TRUE}, \code{n_boot} bootstrap samples are drawn, the g-compuation step is performed on each bootstrap sample and the confidence intervals are calculated using the percentile method from these results. Can get very slow if the dataset is large or there are many values in \code{horizon} or \code{times}. Using \code{n_cores} can speed up the process a lot.
  }
  \item{conf_level}{
A number specifying the confidence level of the bootstrap confidence intervals. Ignored if \code{conf_int=FALSE}.
  }
  \item{n_boot}{
A single integer specifying how many bootstrap repetitions should be performed. Ignored if  \code{conf_int=FALSE}.
  }
  \item{n_cores}{
The number of processor cores to use when performing the calculations. If \code{n_cores=1} (default), single threaded processing is used. If \code{n_cores > 1} the \pkg{foreach} package and the \pkg{doParallel} package are used to run the calculations on \code{n_cores} in parallel. This might speed up the runtime considerably when it is initially slow.
  }
  \item{na.action}{
How missing values should be handled. Can be one of: \code{na.fail}, \code{na.omit}, \code{na.pass}, \code{na.exclude} or a user-defined custom function. Also accepts strings of the function names. See \code{?na.action} for more details. By default it uses the na.action which is set in the global options by the respective user.
  }
  \item{return_boot}{
Either \code{TRUE} or \code{FALSE} (default). If \code{TRUE} (and \code{conf_int=TRUE}) this function will return the individual bootstrap estimates instead of the usual estimates. This may be useful to perform tests or calculate other type of bootstrap confidence intervals manually.
  }
  \item{...}{
Further arguments passed to \code{\link[=riskRegression]{predictRisk}}.
  }
}
\details{

This function is used internally in all plot functions included in this R-package and generally does not need to be called directly by the user. It can however be used to get specific values or as a basis to create custom plots not included in this package. Below we give a small introduction to what this function does. A more detailed description of the underlying methodology can be found in our article on this subject (Denz & Timmesfeld 2022).

\strong{\emph{Target Estimand (default)}}

By default (\code{contrast="none"}) this function tries to estimate the survival probability at \code{times} that would have been observed if every individual in the sample had received a value of \code{horizon} in the \code{variable}. Let \eqn{Z} be the continuous variable we are interested in. Let \eqn{T} be the time until the occurence of the event of interest. Under the potential outcome framework, there is an uncountably infinite amount of potential survival times \eqn{T^{(Z=z)}}, one for each possible value of \eqn{Z}. The target estimand is then defined as:

\deqn{S_{z}(t) = E(I(T^{(Z=z)} > t))}

If we additionally consider a categorical \code{group} variable \eqn{D}, the target estimand is similarly defined as:

\deqn{S_{zd}(t) = E(I(T^{(Z=z, D=d)} > t))}

where \eqn{T^{(Z=z, D=d)}} is the survival time that would have been observed if the individual had received both \eqn{Z = z} and \eqn{D = d}.

\strong{\emph{Target Estimand (using contrasts)}}

If contrasts are used (\code{contrast!="none"}), target estimands based on \eqn{S_{z}(t)} or \eqn{S_{zd}(t)} are used instead. When using \code{contrast="diff"} and \code{reference="km"}, the target estimand is simply the difference between the observed survival probability in the entire sample (denoted by \eqn{S(t)}, estimated using a Kaplan-Meier estimator) and the counterfactual survival probability:

\deqn{\Delta_{KM}(t, z) = S(t) - S_z(t)}

If \code{contrast="ratio"} is used instead, the substraction sign is simply replace by a division. If \code{group} was specified, \eqn{S(t)} is replaced by \eqn{S_d(t)} (a stratified Kaplan-Meier estimator) and \eqn{S_z(t)} is replaced by \eqn{S_{zd}(t)}. Instead of using a Kaplan-Meier estimator as \code{reference}, one may also use a specific \eqn{S_z(t)} as reference using \code{reference="value"} and setting \code{ref_value} to the \eqn{Z} that should be used. All of these causal contrasts and their implications are described in detail in the appendix of our article on this topic (Denz & Timmesfeld 2022).

\strong{\emph{Estimation Methodology}}

\emph{G-Computation}, also known as the \emph{Corrected Group Prognosis} method, \emph{Direct-Standardization} or \emph{G-Formula}, is used internally to estimate the counterfactual survival probability or CIF for values of a continuous variable. This is done by setting the \code{variable} to a specific value for all rows in \code{data} first. Afterwards, the \code{model} is used to predict the survival probability of each individual at all \code{times}, given the value of \code{variable} and their other observed covariates (which are included in the model as independent variables). These estimates are then averaged for each time point. This procedure is repeated for every value in \code{horizon}. If a \code{group} is supplied, these calculations are repeated for every posisble value in \code{group}, with the estimated individual survival probabilities also being conditional on that value.

To obtain valid estimates of the target estimand using this function, the fundamental causal identifiability assumptions have to be met. Those are described in detail in our article on this topic (Denz & Timmesfeld 2022). If those assumptions are not met, the estimates may only be used to showcase simple associations. They cannot be endowed with a counterfactual interpretation in this case.

\strong{\emph{Supported Models}}

This function relies on the \code{\link[=riskRegression]{predictRisk}} function from the \pkg{riskRegression} package to create the covariate and time specific estimates of the probabilities. All models with an associated \code{predictRisk} method may be used in this function. This includes a variety of models, such as the Cox proportional hazards regression model and the aalen additive hazards model. If the model that should be used has no \code{predictRisk} method, the user either needs to write their own \code{predictRisk} method or contact the maintainers of the \pkg{riskRegression} package.

\strong{\emph{Bootstrap Confidence Intervals}}

By using \code{conf_int=TRUE}, bootstrap confidence intervals may be estimated. This will draw \code{n_boot} samples of size \code{nrow(data)} with replacement from \code{data} in the first step. Afterwards, the \code{model} is fit to each of those bootstrap samples and the \code{curve_cont} function is recursively called to obtain the estimates of interests for each sample. Using the percentile approach, the bootstrap confidence intervals are calculated. This requires that the \code{model} object contains a \code{call} parameter, because the \code{update()} function is used internally.

\strong{\emph{Computational Complexity}}

If many values are included in \code{times}, \code{horizon} or both and/or \code{data} has a lot of rows, this function may become very slow. Since bootstrapping relies on sampling with replacement from the original \code{data} and repeating the entire procedure \code{n_boot} times, this also dramatically increases the runtime. Parallel processing may be used to speed up the computations. This can be done by simply setting \code{n_cores} to values higher than 1.

\strong{\emph{Missing Data}}

Currently, this function does not support advanced handling of missing data. The \code{data.frame} supplied to \code{data} should contain no missing data in the relevant columns. To achieve this, the \code{na.action} argument can be set to \code{"na.omit"}, which will remove all rows that do contain missing data.

\strong{\emph{Competing Events}}

By supplying a \code{model} that directly takes into account competing events and using the \code{cause} argument, the user may also use the functionality offered in this package to create plots in this setting. Internally, the \code{predictRisk} method will then be used to estimate the conditional cause-specific cumulative incidence function, which will then be used to carry out the g-computation step explained above. \strong{However} the underlying target estimand of this procedure is dependent on which kind of model was supplied. It is therefore not possible to define it here concisely. Future research is necessary to clarify this point. This feature should only be used with great caution.

}
\value{
Returns a \code{data.frame} containing the columns \code{time} (the point in time), \code{est} (the estimated survival probability or CIF) and \code{cont} (the specific value of \code{variable} used). If \code{group} was used, it includes the additional \code{group} column, specifying the level of the grouping variable. If \code{conf_int=TRUE} was used, it additionally includes the columns \code{se} (bootstrap standard error of the estimate), \code{ci_lower} (lower limit of the bootstrap confidence interval) and \code{ci_upper} (upper limit of the bootstrap confidence interval).
}
\references{
Robin Denz, Nina Timmesfeld (2022). Visualizing the Causal Effect of a Continuous Variable on a Time-To-Event Outcome. arXiv:2208.04644v1

Brice Ozenne, Anne Lyngholm Sorensen, Thomas Scheike, Christian Torp-Pedersen and Thomas Alexander Gerds. riskRegression: Predicting the Risk of an Event using Cox Regression Models. The R Journal (2017) 9:2, pages 440-460.

I-Ming Chang, Rebecca Gelman, and Marcello Pagano. Corrected Group Prognostic Curves and Summary Statistics. Journal of Chronic Diseases (1982) 35, pages 669-674

James Robins. A New Approach to Causal Inference in Mortality Studies with a Sustained Exposure Period: Application to Control of the Healthy Worker Survivor Effect. Mathematical Modelling (1986) 7, pages 1393-1512.
}
\author{
Robin Denz
}
\seealso{
\code{\link[=riskRegression]{predictRisk}}
}
\examples{
library(contsurvplot)
library(riskRegression)
library(survival)

# using data from the survival package
data(nafld, package="survival")

# take a random sample to keep example fast
set.seed(42)
nafld1 <- nafld1[sample(nrow(nafld1), 150), ]

# fit cox-model with age
model <- coxph(Surv(futime, status) ~ age, data=nafld1, x=TRUE)

# estimate survival probability at some points in time, for
# a range of age values
plotdata <- curve_cont(data=nafld1,
                       variable="age",
                       model=model,
                       horizon=c(50, 60, 70, 80),
                       times=c(1000, 2000, 3000, 4000))

# estimate cumulative incidences instead
plotdata <- curve_cont(data=nafld1,
                       variable="age",
                       model=model,
                       horizon=c(50, 60, 70, 80),
                       times=c(1000, 2000, 3000, 4000),
                       cif=TRUE)
}
