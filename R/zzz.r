
.onAttach <- function(libname, pkgname) {
  cit <- paste0('Denz R, Timmesfeld N (2023). "Visualizing the',
                " (Causal) Effect of a Continuous Variable on a",
                ' Time-To-Event Outcome." Epidemiology, 34 (5).',
                " doi: 10.1097/EDE.0000000000001630.")
  packageStartupMessage("Please cite as: \n\n", cit)
}
