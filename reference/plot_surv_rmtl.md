# Plot the Effect of a Continuous Variable on the Restricted Mean Time Lost

Using a previously fit time-to-event model, this function plots the
restricted mean time lost (RMTL) as a function of a continuous variable.
Unlike the
[`plot_surv_rmst`](https://robindenz1.github.io/contsurvplot/reference/plot_surv_rmst.md)
function, this function can deal with multiple event types in the
`status` variable.

## Usage

``` r
plot_surv_rmtl(time, status, variable, group=NULL, cause=1,
               data, model, conf_int=FALSE, conf_level=0.95,
               n_boot=300, na.action=options()$na.action,
               tau, horizon=NULL, custom_colors=NULL,
               size=1, linetype="solid", alpha=1, color="black",
               xlab=variable, ylab="Restricted Mean Time Lost",
               title=NULL, subtitle=NULL,
               legend.title="tau", legend.position="right",
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
  interest, for which the CIFs should be estimated. This variable has to
  be contained in the `data.frame` that is supplied to the `data`
  argument.

- group:

  An optional single character string specifying a factor variable in
  `data`. When used, the plot is created conditional on this factor
  variable, meaning that a facetted plot is produced with one facet for
  each level of the factor variable. See
  [`curve_cont`](https://robindenz1.github.io/contsurvplot/reference/curve_cont.md)
  for a detailed description of the estimation strategy. Set to `NULL`
  (default) to use no grouping variable.

- cause:

  The cause of interest. In standard survival data with only one event
  type, this should be kept at 1. For data with multiple failure types,
  this argument should be specified.

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

- tau:

  The point in time to which the RMTL should be calculated. Can be a
  vector of numbers. If multiple values are supplied, one curve is drawn
  for each of them.

- horizon:

  A numeric vector containing a range of values of `variable` for which
  the CIFs should be calculated or `NULL` (default). If `NULL`, the
  horizon is constructed as a sequence from the lowest to the highest
  value observed in `variable` with 100 equally spaced steps.

- custom_colors:

  An optional character vector of colors to use when there are multiple
  values in `tau`. Ignored if `length(tau)==1`, in which case the
  `color` argument can be used to specify the color of the single line.

- size:

  A single number specifying how thick the lines should be drawn.

- linetype:

  The linetype of the drawn lines. See documentation of ggplot2 for more
  details on allowed values.

- alpha:

  The transparency level of the lines.

- color:

  The color of the curve if `tau` is a single value. If a numeric vector
  was supplied to `tau`, use the `custom_colors` argument to specify
  them instead.

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

This function is essentially equal to the `plot_surv_rmtl` function. The
only difference is that instead of using the restricted mean survival
time (RMST), which is the area under the survival curve up to a specific
point in time `tau`, this function uses the restricted mean time lost
(RMTL), which is the area under the (cause-specific) CIF up to a
specific point in time `tau`. It can be interpreted as the mean time it
takes an individual to succumb to the event of interest before `tau`.

The reason this is split into two functions is, that the RMTL can be
calculated when mutually exclusive competing events are present, while
the RMST cannot. The basic pros and cons of the
[`plot_surv_rmst`](https://robindenz1.github.io/contsurvplot/reference/plot_surv_rmst.md)
function still apply here, however. For more information we suggest
consulting the documentation page of the
[`plot_surv_rmst`](https://robindenz1.github.io/contsurvplot/reference/plot_surv_rmst.md)
function.

## Value

Returns a `ggplot2` object.

## Author

Robin Denz

## References

Eng, K. H.; Schiller, E. & Morrell, K. On Representing the Prognostic
Value of Continuous Gene Expression Biomarkers with the Restricted Mean
Survival Curve. In: Oncotarget, 2015, 6, 36308-36318

Robin Denz, Nina Timmesfeld (2023). "Visualizing the (Causal) Effect of
a Continuous Variable on a Time-To-Event Outcome". In: Epidemiology 34.5

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

# plot effect of age on the RMST for ages 50 to 80
plot_surv_rmtl(time="futime",
               status="status",
               variable="age",
               data=nafld1,
               model=model,
               horizon=seq(50, 80, 1),
               tau=2500)


# plot RMST for multiple tau values for ages 50 to 80
plot_surv_rmtl(time="futime",
               status="status",
               variable="age",
               data=nafld1,
               model=model,
               horizon=seq(50, 80, 1),
               tau=c(2000, 3000, 5000))


## showing non-linear effects

# fit cox-model with bmi modeled using B-Splines,
# adjusting for age and sex
model2 <- coxph(Surv(futime, status) ~ age + male + bs(bmi, df=3),
                data=nafld1, x=TRUE)

# plot effect of bmi on survival
plot_surv_rmtl(time="futime",
               status="status",
               variable="bmi",
               data=nafld1,
               model=model2,
               tau=c(2000, 3000, 5000))
```
