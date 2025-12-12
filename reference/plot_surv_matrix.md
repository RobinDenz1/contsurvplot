# Plot a Discretized Heatmap of the Effect of a Continuous Covariate on a Time-To-Event Outcome

Using a previously fit time-to-event model, this function plots a
discretized heatmap with the continuous covariate on the y-axis and the
time-to-event on the x-axis. This is essentially a discretized version
of the
[`plot_surv_heatmap`](https://robindenz1.github.io/contsurvplot/reference/plot_surv_heatmap.md)
plot, which makes it look very similar to a correlation matrix.

## Usage

``` r
plot_surv_matrix(time, status, variable, group=NULL, data, model,
                 cif=FALSE, na.action=options()$na.action,
                 horizon=NULL, fixed_t=NULL, max_t=Inf,
                 n_col=10, n_row=10,
                 start_color="red", end_color="blue",
                 alpha=1, xlab="Time", ylab=variable,
                 title=NULL, subtitle=NULL,
                 legend.title="S(t)", legend.position="none",
                 gg_theme=ggplot2::theme_bw(),
                 facet_args=list(),
                 panel_border=FALSE, axis_dist=0,
                 border_color="white", border_size=0.5,
                 numbers=TRUE, number_color="white",
                 number_size=3, number_family="sans",
                 number_fontface="plain", number_digits=2,
                 ...)
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
  steps. In this function, this needs to be a equally spaced vector.

- fixed_t:

  A numeric vector containing points in time at which the survival
  probabilities should be calculated or `NULL` (default). If `NULL`, the
  survival probability is estimated at 100 equally spaced steps from 0
  to the maximum observed event time. In this function, this needs to be
  a equally spaced vector.

- max_t:

  A number indicating the latest survival time which is to be plotted.

- n_col:

  The number of columns to use in the matrix style heatmap. This
  parameter only controls how many tiles are shown, not how many are
  estimated. The amount of estimated probabilities is controlled using
  the `fixed_t` and `horizon` arguments. See details.

- n_row:

  The number of rows to use in the matrix style heatmap. See `n_col`.

- start_color:

  The color used for the lowest value in `horizon`. This and the
  `end_color` argument can be used to specify custom continuous color
  scales used in the plot. For example, if a black and white plot is
  desired, the user can set `start_color="white"` and
  `end_color="black"`. See
  [`?scale_color_gradient`](https://ggplot2.tidyverse.org/reference/scale_gradient.html)
  for more information. Defaults to `"red"`, set to `NULL` to use the
  default ggplot2 palette.

- end_color:

  The color used for the highest value in `horizon`. Defaults to
  `"blue"`. See argument `start_color`.

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

- panel_border:

  Whether to draw a border around the heatmap or not. Is set to FALSE by
  default to mimic standard heatmaps.

- axis_dist:

  The distance of the axis ticks to the colored heatmap. Is set to 0 by
  default to mimic standard heatmaps.

- border_color:

  The color of the individual rectangles borders. Defaults to `"white"`.

- border_size:

  The size of the individual rectangles borders. Defaults to 0.5.

- numbers:

  Whether to put the numbers of the average estimated probabilities into
  the rectangles or not. Defaults to `TRUE`.

- number_color:

  The color of the numbers inside the rectangles. Ignored if
  `numbers=FALSE`.

- number_size:

  The size of the numbers inside the rectangles. Ignored if
  `numbers=FALSE`.

- number_family:

  The font family of the numbers inside the rectangles. Ignored if
  `numbers=FALSE`.

- number_fontface:

  The fontface of the numbers inside the rectangles. Ignored if
  `numbers=FALSE`.

- number_digits:

  The amount of digits the numbers inside the rectangles should be
  rounded to. Ignored if `numbers=FALSE`.

- ...:

  Further arguments passed to
  [`curve_cont`](https://robindenz1.github.io/contsurvplot/reference/curve_cont.md).

## Details

Heatmaps are a great tool to visualize a three dimensional surface in a
two-dimensional plot. Continuously changing colors over a single area
can, however, be difficult to interpret correctly if the color does not
change a lot. This version makes the heatmap easier to understand by
discretizing the space into equally spaced rectangles. The survival or
failure probability is still estimated at a very fine grid of points in
time (controlled using the `fixed_t` and `horizon` arguments), but is
then aggregated into average probabilities afterwards. The number inside
each rectangle then shows the \*average\* probability inside the region
defined by the rectangle. This makes the plot look a lot like a
correlation matrix.

The dimensions of the plot can be controlled using the `n_col` and
`n_row` arguments. Using high numbers in these parameters makes the plot
look more similar to a standard
[`plot_surv_heatmap`](https://robindenz1.github.io/contsurvplot/reference/plot_surv_heatmap.md)
plot. It is recommended to create the
[`plot_surv_heatmap`](https://robindenz1.github.io/contsurvplot/reference/plot_surv_heatmap.md)
plot first to pick appropriate dimensions for the discretized version
here.

## Value

Returns a `ggplot2` object.

## Author

Robin Denz

## References

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

# plot effect of age on survival using defaults
plot_surv_matrix(time="futime",
                 status="status",
                 variable="age",
                 data=nafld1,
                 model=model)


# plot it only for 60 to 80 year old people
plot_surv_matrix(time="futime",
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
plot_surv_matrix(time="futime",
                 status="status",
                 variable="bmi",
                 data=nafld1,
                 model=model2)

```
