# contsurvplot 0.1.0

* This is the first release of this package
* Added a reference to the main paper to the description file

# contsurvplot 0.2.0

Enhancements:

* Installation instructions in vignette and README were updated to include CRAN
* Re-worked the documentation page of the `curve_cont` function 

Bug fixes:

* plot_surv_animated now uses `gganimate::transition_manual()` internally, ruling out interpolation errors
* There are no slight differences in the outlines of the areas when using `plot_surv_area` with `discrete=TRUE` and `discrete=FALSE` anymore

New Features:

* Added `plot_surv_matrix` function
* Added support for bootstrap standard error and confidence interval calculation in the `curve_cont` function
* Added support for causal contrasts in `curve_cont` function, which also makes it possible to use those directly in all plot functions
* Added support for bootstrap confidence intervals in the `plot_surv_at_t`, `plot_surv_lines` and `plot_surv_animated` functions
* Added support for plotting Kaplan-Meier curves as reference in `plot_surv_area`, `plot_surv_animated`, `plot_surv_lines`
* Added the `monotonic` argument in the `plot_surv_area` function to allow plotting curved relationships
* Vignette now includes `plot_surv_matrix` function
