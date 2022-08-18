\name{plot_surv_tiles}
\alias{plot_surv_tiles}

\title{
Plot a Discretized Heatmap of the Effect of a Continuous Covariate on a Time-To-Event Outcome
}
\description{
Using a previously fit time-to-event model, this function plots a discretized heatmap with the continuous covariate on the y-axis and the time-to-event on the x-axis. This is essentially a discretized version of the \code{\link{plot_surv_heatmap}} plot, which makes it look very similar to a correlation matrix.
}
\usage{
plot_surv_tiles(time, status, variable, data, model,
                cif=FALSE, na.action=options()$na.action,
                horizon=NULL, fixed_t=NULL, max_t=Inf,
                n_col=10, n_row=10,
                start_color="red", end_color="blue",
                alpha=1, xlab="Time", ylab=variable,
                title=NULL, subtitle=NULL,
                legend.title="S(t)", legend.position="none",
                gg_theme=ggplot2::theme_bw(),
                panel_border=FALSE, axis_dist=0,
                border_color="white", border_size=0.5,
                numbers=TRUE, number_color="white",
                number_size=3, number_family="sans",
                number_fontface="plain", number_digits=2,
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
A model describing the time-to-event process (such as an \code{coxph} model). Needs to include \code{variable} as an independent variable. It also has to have an associated \code{\link[=riskRegression]{predictRisk}} method. See \code{?predictRisk} for more details.
  }
  \item{cif}{
Whether to plot the cumulative incidence (CIF) instead of the survival probability. If multiple failure types are present, the survival probability cannot be estimated in an unbiased way. This function will always return CIF estimates in that case.
  }
  \item{na.action}{
How missing values should be handled. Can be one of: \code{na.fail}, \code{na.omit}, \code{na.pass}, \code{na.exclude} or a user-defined custom function. Also accepts strings of the function names. See \code{?na.action} for more details. By default it uses the na.action which is set in the global options by the respective user.
  }
  \item{horizon}{
A numeric vector containing a range of values of \code{variable} for which the survival curves should be calculated or \code{NULL} (default). If \code{NULL}, the horizon is constructed as a sequence from the lowest to the highest value observed in \code{variable} with 100 equally spaced steps. In this function, this needs to be a equally spaced vector.
  }
  \item{fixed_t}{
A numeric vector containing points in time at which the survival probabilities should be calculated or \code{NULL} (default). If \code{NULL}, the survival probability is estimated at 100 equally spaced steps from 0 to the maximum observed event time. In this function, this needs to be a equally spaced vector.
  }
  \item{max_t}{
A number indicating the latest survival time which is to be plotted.
  }
  \item{n_col}{
The number of columns to use in the matrix style heatmap. This parameter only controls how many tiles are shown, not how many are estimated. The amount of estimated probabilities is controlled using the \code{fixed_t} and \code{horizon} arguments. See details.
  }
  \item{n_row}{
The number of rows to use in the matrix style heatmap. See \code{n_col}.
  }
  \item{start_color}{
The color used for the lowest value in \code{horizon}. This and the \code{end_color} argument can be used to specify custom continuous color scales used in the plot. For example, if a black and white plot is desired, the user can set \code{start_color="white"} and \code{end_color="black"}. See \code{?scale_color_gradient} for more information. Defaults to \code{"red"}, set to \code{NULL} to use the default \pkg{ggplot2} palette.
  }
  \item{end_color}{
The color used for the highest value in \code{horizon}. Defaults to \code{"blue"}. See argument \code{start_color}.
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
  \item{border_color}{
The color of the individual rectangles borders. Defaults to \code{"white"}.
  }
  \item{border_size}{
The size of the individual rectangles borders. Defaults to 0.5.
  }
  \item{numbers}{
Whether to put the numbers of the average estimated probabilities into the rectangles or not. Defaults to \code{TRUE}.
  }
  \item{number_color}{
The color of the numbers inside the rectangles. Ignored if \code{numbers=FALSE}.
  }
  \item{number_size}{
The size of the numbers inside the rectangles. Ignored if \code{numbers=FALSE}.
  }
  \item{number_family}{
The font family of the numbers inside the rectangles. Ignored if \code{numbers=FALSE}.
  }
  \item{number_fontface}{
The fontface of the numbers inside the rectangles. Ignored if \code{numbers=FALSE}.
  }
  \item{number_digits}{
The amount of digits the numbers inside the rectangles should be rounded to. Ignored if \code{numbers=FALSE}.
  }
  \item{...}{
Further arguments passed to \code{\link{curve_cont}}.
  }
}
\details{
Heatmaps are a great tool to visualize a three dimensional surface in a two-dimensional plot. Continuously changing colors over a single area can, however, be difficult to interpret correctly if the color does not change a lot. This version makes the heatmap easier to understand by discretizing the space into equally spaced rectangles. The survival or failure probability is still estimated at a very fine grid of points in time (controlled using the \code{fixed_t} and \code{horizon} arguments), but is then aggregated into average probabilities afterwards. The number inside each rectangle then shows the *average* probability inside the region defined by the rectangle. This makes the plot look a lot like a correlation matrix.

The dimensions of the plot can be controlled using the \code{n_col} and \code{n_row} arguments. Using high numbers in these parameters makes the plot look more similar to a standard \code{\link{plot_surv_heatmap}} plot. It is recommended to create the \code{\link{plot_surv_heatmap}} plot first to pick appropriate dimensions for the discretized version here.
}
\value{
Returns a \code{ggplot2} object.
}
\author{
Robin Denz
}
\examples{
library(contsurvplot)
library(riskRegression)
library(survival)
library(ggplot2)
library(splines)

# using data from the survival package
data(nafld, package="survival")

# take a random sample to keep example fast
set.seed(42)
nafld1 <- nafld1[sample(nrow(nafld1), 150), ]

# fit cox-model with age
model <- coxph(Surv(futime, status) ~ age, data=nafld1, x=TRUE)

# plot effect of age on survival using defaults
plot_surv_tiles(time="futime",
                status="status",
                variable="age",
                data=nafld1,
                model=model)

# plot it only for 60 to 80 year old people
plot_surv_tiles(time="futime",
                status="status",
                variable="age",
                data=nafld1,
                model=model,
                horizon=seq(60, 80, 0.5))

## showing non-linear effects

# fit cox-model with bmi modelled using B-Splines,
# adjusting for age and sex
model2 <- coxph(Surv(futime, status) ~ age + male + bs(bmi, df=3),
                data=nafld1, x=TRUE)

# plot effect of bmi on survival using defaults
plot_surv_tiles(time="futime",
                status="status",
                variable="bmi",
                data=nafld1,
                model=model2)

}
