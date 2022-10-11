# contsurvplot 0.1.0

* This is the first release of this package
* Added a reference to the main paper to the description file

# contsurvplot 0.2.0

Enhancements:

* Installation instructions in vignette and README were updated to include CRAN

Bug fixes:

* plot_surv_animated now uses gganimate::transition_manual() internally, ruling out interpolation errors

New Features:

* Added plot_surv_matrix function
* Added support for bootstrap standard error and confidence interval calculation in the curve_cont function
* Added support for bootstrap confidence intervals in the plot_surv_at_t, plot_surv_lines, plot_surv_animated functions
* Vignette now includes plot_surv_matrix function
