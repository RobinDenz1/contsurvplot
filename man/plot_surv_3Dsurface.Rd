\name{plot_surv_3Dsurface}
\alias{plot_surv_3Dsurface}

\title{
Plot the Survival Curve or CIF Dependent on a Continuous Variable as a 3D Surface
}
\description{
Using a previously fit time-to-event model, this function plots the survival curve as a function of a continuous variable using a 3D surface representation. This function can produce interactive plots using the \pkg{plotly} package, or a static image using the \code{persp} function of the \pkg{graphics} package.
}
\usage{
plot_surv_3Dsurface(time, status, variable, data, model,
                    cif=FALSE, na.action=options()$na.action,
                    horizon=NULL, fixed_t=NULL, max_t=Inf,
                    interactive=FALSE,
                    xlab="Time", ylab="Survival Probability",
                    zlab=variable, ticktype="detailed",
                    theta=120, phi=20, col="green", shade=0.5,
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
  \item{interactive}{
Whether to draw the 3D surface as a static plot (\code{interactive=FALSE}, the default) or as an interactive plot. When \code{interactive=TRUE} is used, this function relies on the \pkg{plotly} package. In this case, the only aesthetic arguments that still work are the \code{xlab}, \code{ylab} and \code{zlab} arguments. Everything else has to be set manually using the functionality of the \pkg{plotly} package directly.
  }
  \item{xlab}{
A character string used as the x-axis label of the plot, representing the survival time.
  }
  \item{ylab}{
A character string used as the y-axis label of the plot, representing the survival probability or CIF.
  }
  \item{zlab}{
A character string used as the z-axis of the plot, representing the continuous covariate of interest.
  }
  \item{ticktype}{
A single character: \code{"simple"} draws just an arrow parallel to the axis to indicate direction of increase; \code{"detailed"} draws normal ticks as per 2D plots. Passed to the \code{ticktype} argument in the \code{persp} function. Ignored if \code{interactive=TRUE}.
  }
  \item{theta}{
Angles defining the viewing direction. \code{theta} gives the azimuthal direction and \code{phi} the colatitude. Passed to the \code{theta} argument in the \code{persp} function. Ignored if \code{interactive=TRUE}.
  }
  \item{phi}{
See argument \code{theta}. Passed to the \code{phi} argument in the \code{persp} function. Ignored if \code{interactive=TRUE}.
  }
  \item{col}{
The color(s) of the surface facets. Transparent colours are ignored. This is recycled to the \code{(nx-1)(ny-1)} facets. Passed to the \code{col} argument in the \code{persp} function. Ignored if \code{interactive=TRUE}.
  }
  \item{shade}{
The shade at a surface facet is computed as \code{((1+d)/2)^shade}, where d is the dot product of a unit vector normal to the facet and a unit vector in the direction of a light source. Values of shade close to one yield shading similar to a point light source model and values close to zero produce no shading. Values in the range 0.5 to 0.75 provide an approximation to daylight illumination. Passed to the \code{shade} argument in the \code{persp} function. Ignored if \code{interactive=TRUE}.
  }
  \item{...}{
Further arguments passed to \code{predictRisk}.
  }
}
\details{
The survival curve dependent on a continuous variable can be viewed as a 3D surface. All other plots in this package try to visualize this surface by reducing it to two dimenstions using color scales. This function on the other hand directly plots the 3D surface as such.

Although 3D plots are frowned upon by many scientists, it might be a good visualisation choice from time to time. Using the \code{interactive=TRUE} option makes the plot interactive, which lessens some of the valid criticism of 3D graphics.
}
\value{
Returns a \code{plotly} object if \code{interactive=TRUE} is used and a matrix when \code{interactive=FALSE} is used (still drawing the plot).
}
\author{
Robin Denz
}
\examples{
\dontrun{

}
}
