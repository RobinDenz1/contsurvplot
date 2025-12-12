# Estimate Counterfactual Survival or Failure Probabilities for Levels of a Continuous Variable

This function can be utilized to estimate counterfactual survival curves
or cumulative incidence functions (CIF) for specific values of a
continuous covariate.

## Usage

``` r
curve_cont(data, variable, model, horizon,
           times, group=NULL, cause=1, cif=FALSE,
           contrast="none", reference="km", ref_value=NULL,
           event_time=NULL, event_status=NULL,
           conf_int=FALSE, conf_level=0.95,
           n_boot=300, n_cores=1,
           na.action=options()$na.action,
           return_boot=FALSE, ...)
```

## Arguments

- data:

  A `data.frame` containing all required variables.

- variable:

  A single character string specifying the continuous variable of
  interest, for which the survival curves should be estimated. This
  variable has to be contained in the `data.frame` that is supplied to
  the `data` argument.

- model:

  A model describing the time-to-event process (such as an `coxph`
  model). Needs to include `variable` as an independent variable. It
  also has to have an associated `predictRisk` method. See
  [`?predictRisk`](https://rdrr.io/pkg/riskRegression/man/predictRisk.html)
  for more details.

- horizon:

  A numeric vector containing a range of values of `variable` for which
  the survival curves should be calculated.

- times:

  A numeric vector containing points in time at which the survival
  probabilities should be calculated.

- group:

  An optional single character string specifying a factor variable in
  `data`. When used, the regression standardization is performed
  conditional on this factor variable, meaning that one estimate is
  returned for each level of the factor variable. See details for a
  better description. Set to `NULL` (default) to use no grouping
  variable.

- cause:

  The cause of interest. In standard survival data with only one event
  type, this should be kept at 1. For data with multiple failure types,
  this argument should be specified. In addition, the `cif` argument
  should be set to `TRUE` in those cases.

- cif:

  Whether to calculate the cumulative incidence (CIF) instead of the
  survival probability. If multiple failure types are present, the
  survival probability cannot be estimated in an unbiased way. In those
  cases, this argument should always be set to `TRUE`.

- contrast:

  Defines what kind of estimate should be returned. Can be either
  `"none"` (default), `"diff"` or `"ratio"`. When `"none"` is used, it
  simply returns the counterfactual survival probabilities (or CIF if
  `cif=TRUE`) without any further calculations. If `"diff"` or `"ratio"`
  is used instead, the difference or ratio to some reference value will
  be returned. See argument below.

- reference:

  Defines what kind of reference value to use when estimating causal
  contrasts. Only used if `contrast!="none"`, ignored otherwise. Can be
  either `"km"` (using standard Kaplan-Meier estimates as reference) or
  `"value"` (using the g-computation estimates of a specific value of
  the `variable` as reference). To specify which value to use when using
  `reference="value"`, the `ref_value` argument should be used. See
  details for more information.

- ref_value:

  A single number corresponding to the reference value used when
  estimating causal contrasts using `reference="value"`. See details.

- event_time:

  A single character string specifying the time until the occurrence of
  the event of interest or `NULL` (default). Only used when estimating
  causal contrasts with `reference="km"`, ignored otherwise. If
  specified, this variable has to be contained in the `data.frame` that
  is supplied to the `data` argument.

- event_status:

  A single character string specifying the status of the event of
  interest or `NULL` (default). Only used when estimating causal
  contrasts with `reference="km"`, ignored otherwise. If specified, this
  variable has to be contained in the `data.frame` that is supplied to
  the `data` argument.

- conf_int:

  Whether to calculate point-wise confidence intervals or not. If
  `TRUE`, `n_boot` bootstrap samples are drawn, the g-computation step
  is performed on each bootstrap sample and the confidence intervals are
  calculated using the percentile method from these results. Can get
  very slow if the dataset is large or there are many values in
  `horizon` or `times`. Using `n_cores` can speed up the process a lot.

- conf_level:

  A number specifying the confidence level of the bootstrap confidence
  intervals. Ignored if `conf_int=FALSE`.

- n_boot:

  A single integer specifying how many bootstrap repetitions should be
  performed. Ignored if `conf_int=FALSE`.

- n_cores:

  The number of processor cores to use when performing the calculations.
  If `n_cores=1` (default), single threaded processing is used. If
  `n_cores > 1` the foreach package and the doParallel package are used
  to run the calculations on `n_cores` in parallel. This might speed up
  the runtime considerably when it is initially slow.

- na.action:

  How missing values should be handled. Can be one of: `na.fail`,
  `na.omit`, `na.pass`, `na.exclude` or a user-defined custom function.
  Also accepts strings of the function names. See
  [`?na.action`](https://rdrr.io/r/stats/na.action.html) for more
  details. By default it uses the na.action which is set in the global
  options by the respective user.

- return_boot:

  Either `TRUE` or `FALSE` (default). If `TRUE` (and `conf_int=TRUE`)
  this function will return the individual bootstrap estimates instead
  of the usual estimates. This may be useful to perform tests or
  calculate other type of bootstrap confidence intervals manually.

- ...:

  Further arguments passed to
  [`predictRisk`](https://rdrr.io/pkg/riskRegression/man/predictRisk.html).

## Details

This function is used internally in all plot functions included in this
R-package and generally does not need to be called directly by the user.
It can however be used to get specific values or as a basis to create
custom plots not included in this package. Below we give a small
introduction to what this function does. A more detailed description of
the underlying methodology can be found in our article on this subject
(Denz & Timmesfeld 2022).

***Target Estimand (default)***

By default (`contrast="none"`) this function tries to estimate the
survival probability at `times` that would have been observed if every
individual in the sample had received a value of `horizon` in the
`variable`. Let \\Z\\ be the continuous variable we are interested in.
Let \\T\\ be the time until the occurrence of the event of interest.
Under the potential outcome framework, there is an uncountably infinite
amount of potential survival times \\T^{(Z=z)}\\, one for each possible
value of \\Z\\. The target estimand is then defined as:

\$\$S\_{z}(t) = E(I(T^{(Z=z)} \> t))\$\$

If we additionally consider a categorical `group` variable \\D\\, the
target estimand is similarly defined as:

\$\$S\_{zd}(t) = E(I(T^{(Z=z, D=d)} \> t))\$\$

where \\T^{(Z=z, D=d)}\\ is the survival time that would have been
observed if the individual had received both \\Z = z\\ and \\D = d\\.

***Target Estimand (using contrasts)***

If contrasts are used (`contrast!="none"`), target estimands based on
\\S\_{z}(t)\\ or \\S\_{zd}(t)\\ are used instead. When using
`contrast="diff"` and `reference="km"`, the target estimand is simply
the difference between the observed survival probability in the entire
sample (denoted by \\S(t)\\, estimated using a Kaplan-Meier estimator)
and the counterfactual survival probability:

\$\$\Delta\_{KM}(t, z) = S(t) - S_z(t)\$\$

If `contrast="ratio"` is used instead, the substraction sign is simply
replace by a division. If `group` was specified, \\S(t)\\ is replaced by
\\S_d(t)\\ (a stratified Kaplan-Meier estimator) and \\S_z(t)\\ is
replaced by \\S\_{zd}(t)\\. Instead of using a Kaplan-Meier estimator as
`reference`, one may also use a specific \\S_z(t)\\ as reference using
`reference="value"` and setting `ref_value` to the \\Z\\ that should be
used. All of these causal contrasts and their implications are described
in detail in the appendix of our article on this topic (Denz &
Timmesfeld 2022).

***Estimation Methodology***

*G-Computation*, also known as the *Corrected Group Prognosis* method,
*Direct-Standardization* or *G-Formula*, is used internally to estimate
the counterfactual survival probability or CIF for values of a
continuous variable. This is done by setting the `variable` to a
specific value for all rows in `data` first. Afterwards, the `model` is
used to predict the survival probability of each individual at all
`times`, given the value of `variable` and their other observed
covariates (which are included in the model as independent variables).
These estimates are then averaged for each time point. This procedure is
repeated for every value in `horizon`. If a `group` is supplied, these
calculations are repeated for every possible value in `group`, with the
estimated individual survival probabilities also being conditional on
that value.

To obtain valid estimates of the target estimand using this function,
the fundamental causal identifiability assumptions have to be met. Those
are described in detail in our article on this topic (Denz & Timmesfeld
2022). If those assumptions are not met, the estimates may only be used
to showcase simple associations. They cannot be endowed with a
counterfactual interpretation in this case.

***Supported Models***

This function relies on the
[`predictRisk`](https://rdrr.io/pkg/riskRegression/man/predictRisk.html)
function from the riskRegression package to create the covariate and
time specific estimates of the probabilities. All models with an
associated `predictRisk` method may be used in this function. This
includes a variety of models, such as the Cox proportional hazards
regression model and the aalen additive hazards model. If the model that
should be used has no `predictRisk` method, the user either needs to
write their own `predictRisk` method or contact the maintainers of the
riskRegression package.

***Bootstrap Confidence Intervals***

By using `conf_int=TRUE`, bootstrap confidence intervals may be
estimated. This will draw `n_boot` samples of size `nrow(data)` with
replacement from `data` in the first step. Afterwards, the `model` is
fit to each of those bootstrap samples and the `curve_cont` function is
recursively called to obtain the estimates of interests for each sample.
Using the percentile approach, the bootstrap confidence intervals are
calculated. This requires that the `model` object contains a `call`
parameter, because the [`update()`](https://rdrr.io/r/stats/update.html)
function is used internally.

***Computational Complexity***

If many values are included in `times`, `horizon` or both and/or `data`
has a lot of rows, this function may become very slow. Since
bootstrapping relies on sampling with replacement from the original
`data` and repeating the entire procedure `n_boot` times, this also
dramatically increases the runtime. Parallel processing may be used to
speed up the computations. This can be done by simply setting `n_cores`
to values higher than 1.

***Missing Data***

Currently, this function does not support advanced handling of missing
data. The `data.frame` supplied to `data` should contain no missing data
in the relevant columns. To achieve this, the `na.action` argument can
be set to `"na.omit"`, which will remove all rows that do contain
missing data.

***Competing Events***

By supplying a `model` that directly takes into account competing events
and using the `cause` argument, the user may also use the functionality
offered in this package to create plots in this setting. Internally, the
`predictRisk` method will then be used to estimate the conditional
cause-specific cumulative incidence function, which will then be used to
carry out the g-computation step explained above. **However** the
underlying target estimand of this procedure is dependent on which kind
of model was supplied. It is therefore not possible to define it here
concisely. Future research is necessary to clarify this point. This
feature should only be used with great caution.

## Value

Returns a `data.frame` containing the columns `time` (the point in
time), `est` (the estimated survival probability or CIF) and `cont` (the
specific value of `variable` used). If `group` was used, it includes the
additional `group` column, specifying the level of the grouping
variable. If `conf_int=TRUE` was used, it additionally includes the
columns `se` (bootstrap standard error of the estimate), `ci_lower`
(lower limit of the bootstrap confidence interval) and `ci_upper` (upper
limit of the bootstrap confidence interval).

## References

Robin Denz, Nina Timmesfeld (2023). "Visualizing the (Causal) Effect of
a Continuous Variable on a Time-To-Event Outcome". In: Epidemiology 34.5

Brice Ozenne, Anne Lyngholm Sorensen, Thomas Scheike, Christian
Torp-Pedersen and Thomas Alexander Gerds. riskRegression: Predicting the
Risk of an Event using Cox Regression Models. The R Journal (2017) 9:2,
pages 440-460.

I-Ming Chang, Rebecca Gelman, and Marcello Pagano. Corrected Group
Prognostic Curves and Summary Statistics. Journal of Chronic Diseases
(1982) 35, pages 669-674

James Robins. A New Approach to Causal Inference in Mortality Studies
with a Sustained Exposure Period: Application to Control of the Healthy
Worker Survivor Effect. Mathematical Modelling (1986) 7, pages
1393-1512.

## Author

Robin Denz

## See also

[`predictRisk`](https://rdrr.io/pkg/riskRegression/man/predictRisk.html)

## Examples

``` r
library(contsurvplot)
library(riskRegression)
#> riskRegression version 2025.09.17
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
```
