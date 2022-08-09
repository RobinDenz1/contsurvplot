\name{contsurvplot-package}
\alias{contsurvplot-package}

\title{
Visualize the Effect of a Continuous Variable on a Time-To-Event Outcome
}
\description{

\strong{\emph{What is this package about?}}

This package provides different plotting routines to visualize the causal effect of a continuous variable on a time-to-event outcome using a previously fit model and g-computation. Unlike simpler alternatives, such as plotting survival curves for some categories, these plots always correspond to the results obtained by the time-to-event model and give an accurate depiction of the causal effect, if all assumptions are met.

\strong{\emph{What features are included in this package?}}

The package includes 10 different plot functions, most based on the \pkg{ggplot2} package. Those 10 plotting routines include \emph{value specific survival curves}, \emph{landmark survival probability plots}, \emph{survival time quantile plots}, \emph{survival probability heatmaps}, and \emph{survival area plots}, amongst others. A description and comparison of these plots can be found in the article associated with this R-package (Denz & Timmesfeld 2022).

\strong{\emph{What does a typical workflow using this package look like?}}

All the user has to do is fit a time-to-event model, such as the cox-model, including the continuous variable of interest (and possibly confounders) and plug it into one of the plot functions included in this package. Many different kind of models are supported. See \code{\link[contsurvplot]{curve_cont}} for more details.

\strong{\emph{What type of plot should I use?}}

There is no general answer to this question, but we would usually suggest using a plot method that is able to visualize the causal survival probability both as a function of time and as a function of the continuous variable. The \code{\link[contsurvplot]{plot_surv_area}}, \code{\link[contsurvplot]{plot_surv_heatmap}} and \code{\link[contsurvplot]{plot_surv_contour}} functions do just that. More discussion about this topic can be found in the vignette and the associated paper.

\strong{\emph{Where can I get more information?}}

The documentation pages contain a lot of information, relevant examples and literature references. Additional examples can be found in the vignette of this package, which can be accessed using \code{vignette(topic="introduction", package="contsurvplot")}. We are also currently working on an article focused on the two novel plotting methods \code{\link[contsurvplot]{plot_surv_area}} and \code{\link[contsurvplot]{plot_surv_contour}}.

\strong{\emph{I want to suggest a new feature / I want to report a bug. Where can I do this?}}

Bug reports, suggestions and feature requests are highly welcome. Please file an issue on the official github page (<https://github.com/RobinDenz1/contsurvplot>) or contact the author directly using the supplied e-mail address.

}
\references{
Robin Denz, Nina Timmesfeld. (2022) Visualizing the Causal Effect of a Continuous Variable on a Time-To-Event Outcome. ARXIV SOON

James Robins. A New Approach to Causal Inference in Mortality Studies with a Sustained Exposure Period: Application to Control of the Healthy Worker Survivor Effect. Mathematical Modelling (1986) 7, pages 1393-1512.
}
\author{
Robin Denz (robin.denz@rub.de)
}
