# Plot Survival Time Quantiles as a Function of a Continuous Variable

Using a previously fit time-to-event model, this function plots one or
multiple survival time quantiles (such as the median survival time) as a
function of a continuous variable of interest.

## Usage

``` r
plot_surv_quantiles(time, status, variable, group=NULL,
                    data, model, na.action=options()$na.action,
                    p=0.5, horizon=NULL,
                    size=1, linetype="solid", alpha=1,
                    custom_colors=NULL, single_color=NULL,
                    xlab=variable, ylab="Survival Time Quantile",
                    title=NULL, subtitle=NULL,
                    legend.title=variable, legend.position="right",
                    gg_theme=ggplot2::theme_bw(),
                    facet_args=list(), ...)
```

## Arguments

- time:

  A single character string specifying the time-to-event variable. Needs
  to be a valid column name of a numeric variable in `data`.

- status:

  A single character string specifying the status variable, indicating
  if a person has experienced an event or not. Needs to be a valid
  column name of a numeric or logical variable in `data`.

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

- p:

  A numeric vector containing the survival time quantiles of interest.
  For example, if the user is interested in plotting only the median
  survival time `p=0.5` should be used. When multiple values are
  supplied, one curve is drawn for each quantile.

- model:

  A model describing the time-to-event process (such as an `coxph`
  model). Needs to include `variable` as an independent variable. It
  also has to have an associated
  [`predictRisk`](https://rdrr.io/pkg/riskRegression/man/predictRisk.html)
  method. See
  [`?predictRisk`](https://rdrr.io/pkg/riskRegression/man/predictRisk.html)
  for more details.

- na.action:

  How missing values should be handled. Can be one of: `na.fail`,
  `na.omit`, `na.pass`, `na.exclude` or a user-defined custom function.
  Also accepts strings of the function names. See
  [`?na.action`](https://rdrr.io/r/stats/na.action.html) for more
  details. By default it uses the na.action which is set in the global
  options by the respective user.

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

- custom_colors:

  An optional character vector specifying the colors that should be used
  when multiple quantiles were supplied to the `p` argument. To set the
  whole plot or a single curve to one color only, use the `single_color`
  argument instead.

- single_color:

  A single character string specifying the color of all drawn lines.

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

- ...:

  Further arguments passed to
  [`curve_cont`](https://robindenz1.github.io/contsurvplot/reference/curve_cont.md).

## Details

Survival Time Quantiles are a single value summarizing the entire
survival curve. For example, the most prominently used survival time
quantile is the median survival time, which can be interpreted as the
time at which half of the people in the sample are expected to have
experienced the event of interest. This plot shows one or more of these
quantiles as a function of a continuous variable of interest.

To calculate the survival time quantiles, it first calls the
`curve_cont` function to get estimates of the value-specific survival
curves. Afterwards, it uses step function interpolation to read off the
survival time quantile from the estimates.

Although this is a simple way to plot the effect of a continuous
covariate on the survival, it can give a misleading visualization of the
relationship in some situations. Plots that do not use summary
statistics, such as the
[`plot_surv_contour`](https://robindenz1.github.io/contsurvplot/reference/plot_surv_contour.md)
and
[`plot_surv_area`](https://robindenz1.github.io/contsurvplot/reference/plot_surv_area.md)
plots, may be preferable.

## Value

Returns a `ggplot2` object.

## Author

Robin Denz

## Examples

``` r
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

# plot effect of age on the median survival time
plot_surv_quantiles(time="futime",
                    status="status",
                    variable="age",
                    data=nafld1,
                    model=model)
#> Ignoring unknown labels:
#> • fill : "age"
#> Warning: Removed 22 rows containing missing values or values outside the scale range
#> (`geom_step()`).


# plot multiple survival time quantiles
plot_surv_quantiles(time="futime",
                    status="status",
                    variable="age",
                    data=nafld1,
                    model=model,
                    p=c(0.1, 0.25, 0.5, 0.75, 0.9))
#> Ignoring unknown labels:
#> • fill : "age"
#> Warning: Removed 104 rows containing missing values or values outside the scale range
#> (`geom_step()`).


## showing non-linear effects

# fit cox-model with bmi modelled using B-Splines,
# adjusting for age and sex
model2 <- coxph(Surv(futime, status) ~ age + male + bs(bmi, df=3),
                data=nafld1, x=TRUE)

# plot effect of bmi on survival
plot_surv_quantiles(time="futime",
                    status="status",
                    variable="bmi",
                    data=nafld1,
                    model=model2,
                    p=c(0.1, 0.25, 0.5, 0.75, 0.9))
#> Ignoring unknown labels:
#> • fill : "bmi"
#> Warning: Removed 107 rows containing missing values or values outside the scale range
#> (`geom_step()`).
```
