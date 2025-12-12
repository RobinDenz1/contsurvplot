# Plot the Survival Probability or CIF at a Fixed Point in Time as a Function of a Continuous Variable

Using a previously fit time-to-event model, this function plots the
survival probability or CIF at one or multiple user defined points at
time as a function of a continuous variable.

## Usage

``` r
plot_surv_at_t(time, status, variable, group=NULL,
               data, model, cif=FALSE, conf_int=FALSE,
               conf_level=0.95, n_boot=300,
               na.action=options()$na.action, t, horizon=NULL,
               size=1, linetype="solid", alpha=1,
               xlab=variable, ylab="Survival Probability at t",
               title=NULL, subtitle=NULL,
               legend.title="t", legend.position="right",
               gg_theme=ggplot2::theme_bw(),
               facet_args=list(), ci_alpha=0.4, ...)
```

## Arguments

- time:

  A single character string specifying the time-to-event variable. Needs
  to be a valid column name of a variable in `data`.

- status:

  A single character string specifying the status variable, indicating
  if a person has experienced an event or not. Needs to be a valid
  column name of a variable in `data`.

- variable:

  A single character string specifying the continuous variable of
  interest, for which the survival curves should be estimated. This
  variable has to be contained in the `data.frame` that is supplied to
  the `data` argument.

- group:

  An optional single character string specifying a factor variable in
  `data`. When used, the plot is created conditional on this factor
  variable, meaning that a facetted plot is produced with one facet for
  each level of the factor variable. See
  [`curve_cont`](https://robindenz1.github.io/contsurvplot/reference/curve_cont.md)
  for a detailed description of the estimation strategy. Set to `NULL`
  (default) to use no grouping variable.

- data:

  A `data.frame` containing all required variables.

- model:

  A model describing the time-to-event process (such as an `coxph`
  model). Needs to include `variable` as an independent variable. It
  also has to have an associated
  [`predictRisk`](https://rdrr.io/pkg/riskRegression/man/predictRisk.html)
  method. See
  [`?predictRisk`](https://rdrr.io/pkg/riskRegression/man/predictRisk.html)
  for more details.

- cif:

  Whether to plot the cumulative incidence (CIF) instead of the survival
  probability. If multiple failure types are present, the survival
  probability cannot be estimated in an unbiased way. This function will
  always return CIF estimates in that case.

- conf_int:

  Whether to plot point-wise bootstrap confidence intervals or not.

- conf_level:

  A number specifying the confidence level of the bootstrap confidence
  intervals. Ignored if `conf_int=FALSE`.

- n_boot:

  A single integer specifying how many bootstrap repetitions should be
  performed. Ignored if `conf_int=FALSE`.

- na.action:

  How missing values should be handled. Can be one of: `na.fail`,
  `na.omit`, `na.pass`, `na.exclude` or a user-defined custom function.
  Also accepts strings of the function names. See
  [`?na.action`](https://rdrr.io/r/stats/na.action.html) for more
  details. By default it uses the na.action which is set in the global
  options by the respective user.

- t:

  The point in time at which the survival probability should be
  calculated. For example, by setting this parameter to 10, this
  function will display the survival probability at t = 10 over values
  of `variable`. If multiple values are supplied, one curve is drawn for
  each of them.

- horizon:

  A numeric vector containing a range of values of `variable` for which
  the survival curves should be calculated or `NULL` (default). If
  `NULL`, the horizon is constructed as a sequence from the lowest to
  the highest value observed in `variable` with 100 equally spaced
  steps.

- size:

  A single number specifying how thick the lines should be drawn.

- linetype:

  The linetype of the drawn lines. See documentation of ggplot2 for more
  details on allowed values.

- alpha:

  The transparency level of the lines.

- xlab:

  A character string used as the x-axis label of the plot.

- ylab:

  A character string used as the y-axis label of the plot.

- title:

  A character string used as the title of the plot.

- subtitle:

  A character string used as the subtitle of the plot.

- legend.title:

  A character string used as the legend title of the plot.

- legend.position:

  Where to put the legend. See
  [`?theme`](https://ggplot2.tidyverse.org/reference/theme.html) for
  more details.

- gg_theme:

  A ggplot2 theme which is applied to the plot.

- facet_args:

  A named list of arguments that are passed to the
  [`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  function call when creating a plot separated by groups. Ignored if
  `group=NULL`. Any argument except the `facets` argument of the
  [`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  function can be used. For example, if the user wants to allow free
  y-scales, this argument could be set to `list(scales="free_y")`.

- ci_alpha:

  A single number defining the transparency level of the confidence
  interval bands.

- ...:

  Further arguments passed to
  [`curve_cont`](https://robindenz1.github.io/contsurvplot/reference/curve_cont.md).

## Details

By picking a single value of time, we can effectively reduce the
dimensions of the plot by one, resulting in a simple curve plot of the
survival probability or CIF at the specific point in time as it changes
over values of the continuous covariate. By supplying a vector of times
to the `t` argument, multiple such curves can be produced in the same
plot.

This can be a very effective plotting strategy if there are specific
points in time of interest already (even better if those were
pre-defined). It is however a poor strategy if a representation of the
effect on the survival over time is desired. An empirical example of
this strategy using the 5 year risk is given in Shen et al. (2017).

As all plot functions in this package, this plot relies on the
[`curve_cont`](https://robindenz1.github.io/contsurvplot/reference/curve_cont.md)
function to calculate the needed probability estimates.

## Value

Returns a `ggplot2` object.

## Author

Robin Denz

## References

Shen, Y.-M.; Le, L. D.; Wilson, R. & Mansmann, U. Graphical Presentation
of Patient-Treatment Interaction Elucidated by Continuous Biomarkers:
Current Practice and Scope for Improvement Methods of Information in
Medicine, 2017, 56, 13-27

Robin Denz, Nina Timmesfeld (2023). "Visualizing the (Causal) Effect of
a Continuous Variable on a Time-To-Event Outcome". In: Epidemiology 34.5

## Examples

``` r
library(contsurvplot)
library(riskRegression)
library(survival)
library(ggplot2)

# using data from the survival package
data(nafld, package="survival")

# take a random sample to keep example fast
set.seed(42)
nafld1 <- nafld1[sample(nrow(nafld1), 150), ]

# fit cox-model with age
model <- coxph(Surv(futime, status) ~ age, data=nafld1, x=TRUE)

# plot effect of age on survival at t=2000
plot_surv_at_t(time="futime",
               status="status",
               variable="age",
               data=nafld1,
               model=model,
               t=2000)
#> Ignoring unknown labels:
#> • fill : "t"
#> • colour : "t"


# plot it for arbitrary multiple values of t
plot_surv_at_t(time="futime",
               status="status",
               variable="age",
               data=nafld1,
               model=model,
               t=c(1000, 2000, 3200, 5643))
#> Ignoring unknown labels:
#> • fill : "t"
```
