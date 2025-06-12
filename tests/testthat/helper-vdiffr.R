# vdiffr:::print_plot.ggplot adds ggplot2::theme_test(), which destroys our theming defaults
registerS3method(
  "print_plot", "ggplot", function(p, title = "") { print(p) },
  envir = asNamespace("vdiffr")
)

expect_doppelganger <- function(name, p, ...) {
  # Visual testing is currently only done locally (via devtools::test()).
  # This is because we want to .Rbuildignore the _snaps folder, which means
  # they won't be available when R CMD check is run on CI.
  # Suppose we could add another step to the CI workflow to run devtools::test(),
  # but I'm going to wait on that until we agree on a standard (for shiny-workflows).
  testthat::skip_on_ci()
  vdiffr::expect_doppelganger(name, p, ...)
}

# some plots have randomized output
set.seed(101)
