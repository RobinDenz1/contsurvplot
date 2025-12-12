# Plot Individual Survival Curves or CIFs for Specific Values of a Continuous Covariate

Using a previously fit time-to-event model, this function plots survival
curves or CIFs that would have been observed if every individual in the
dataset had been set to specific values of a continuous covariate.

## Usage

``` r
plot_surv_lines(time, status, variable, group=NULL,
                data, model, cif=FALSE, conf_int=FALSE,
                conf_level=0.95, n_boot=300,
                na.action=options()$na.action,
                horizon=NULL, fixed_t=NULL, max_t=Inf,
                discrete=TRUE, custom_colors=NULL,
                start_color="blue", end_color="red",
                size=1, linetype="solid", alpha=1,
                xlab="Time", ylab="Survival Probability",
                title=NULL, subtitle=NULL,
                legend.title=variable, legend.position="right",
                gg_theme=ggplot2::theme_bw(),
                facet_args=list(), ci_alpha=0.4,
                kaplan_meier=FALSE, km_size=0.5,
                km_linetype="solid", km_alpha=1,
                km_color="black", km_ci=FALSE,
                km_ci_type="plain", km_ci_level=0.95,
                km_ci_alpha=0.4, ...)
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

- horizon:

  A numeric vector containing a range of values of `variable` for which
  the survival curves should be calculated or `NULL` (default). If
  `NULL`, the horizon is constructed as a sequence from the lowest to
  the highest value observed in `variable` with 12 equally spaced steps.

- fixed_t:

  A numeric vector containing points in time at which the survival
  probabilities should be calculated or `NULL` (default). If `NULL`, the
  survival probability is estimated at every point in time at which an
  event occurred.

- max_t:

  A number indicating the latest survival time which is to be plotted.

- discrete:

  Whether to use a continuous color scale or a discrete one (default).
  If `FALSE`, the default ggplot2 colors are used.

- custom_colors:

  An optional character vector of colors to use when `discrete=FALSE`.
  Ignored if `discrete=TRUE`, in which case the color gradient can be
  defined using the `start_color` and `end_color` arguments.

- start_color:

  The color used for the lowest value in `horizon`. This and the
  `end_color` argument can be used to specify custom continuous color
  scales used in the plot. For example, if a black and white plot is
  desired, the user can set `start_color="white"` and
  `end_color="black"`. See
  [`?scale_color_gradient`](https://ggplot2.tidyverse.org/reference/scale_gradient.html)
  for more information.

- end_color:

  The color used for the highest value in `horizon`. See argument
  `start_color`.

- size:

  A single number specifying the size of the drawn curves.

- linetype:

  A single character string specifying the linetype of the curves.

- alpha:

  The transparency level of the plot.

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

- kaplan_meier:

  Whether to add a standard Kaplan-Meier estimator to the plot or not.
  If `group` is defined, the Kaplan-Meier estimator will be stratified
  by the grouping variable. If `cif=TRUE` was used, the cumulative
  incidence will be displayed instead of the survival curve.

- km_size:

  The size of the Kaplan-Meier line. Ignored if `kaplan_meier=FALSE`.

- km_linetype:

  The linetype of the Kaplan-Meier line. Ignored if
  `kaplan_meier=FALSE`.

- km_alpha:

  The transparency level of the Kaplan-Meier line. Ignored if
  `kaplan_meier=FALSE`.

- km_color:

  The color of the Kaplan-Meier line. Ignored if `kaplan_meier=FALSE`.

- km_ci:

  Whether to draw a confidence interval around the Kaplan-Meier
  estimates. Ignored if `kaplan_meier=FALSE`.

- km_ci_type:

  Which type of confidence interval to calculate for the Kaplan-Meier
  estimates. Corresponds to the `conf.type` argument in the `survfit`
  function. Ignored if `kaplan_meier=FALSE` or `km_ci=FALSE`.

- km_ci_level:

  Which confidence level to use for the confidence interval of the
  Kaplan-Meier estimates. Ignored if `kaplan_meier=FALSE` or
  `km_ci=FALSE`.

- km_ci_alpha:

  The transparency level of the confidence interval of the Kaplan-Meier
  estimates. Ignored if `kaplan_meier=FALSE` or `km_ci=FALSE`.

- ...:

  Further arguments passed to
  [`curve_cont`](https://robindenz1.github.io/contsurvplot/reference/curve_cont.md).

## Details

A simple plot of multiple covariate-specific survival curves.
Internally, it uses the
[`curve_cont`](https://robindenz1.github.io/contsurvplot/reference/curve_cont.md)
function to calculate the survival curves.

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

# plot effect of age on survival using defaults
plot_surv_lines(time="futime",
                status="status",
                variable="age",
                data=nafld1,
                model=model)
#> Ignoring unknown labels:
#> • fill : "age"


# plot it only for some specific user-defined values
plot_surv_lines(time="futime",
                status="status",
                variable="age",
                data=nafld1,
                model=model,
                horizon=c(40, 52, 63, 81))
#> Ignoring unknown labels:
#> • fill : "age"


## showing non-linear effects

# fit cox-model with bmi modelled using B-Splines,
# adjusting for age and sex
model2 <- coxph(Surv(futime, status) ~ age + male + bs(bmi, df=3),
                data=nafld1, x=TRUE)

# plot effect of bmi on survival
plot_surv_lines(time="futime",
                status="status",
                variable="bmi",
                data=nafld1,
                model=model2,
                horizon=c(20, 30, 40))
#> Ignoring unknown labels:
#> • fill : "bmi"
```
