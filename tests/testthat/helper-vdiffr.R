# vdiffr ignores failures when
#   - VDIFFR_RUN_TESTS is "false" (on Travis CI with older versions and dev version of R)
#   - CI is not set (on CRAN)

# vdiffr:::print_plot.ggplot adds ggplot2::theme_test(), which destroys our theming defaults
assignInNamespace("print_plot.ggplot", function(p, title = "") { print(p) }, "vdiffr")

expect_doppelganger <- function(name, p, ...) {
  vdiffr::expect_doppelganger(name, p, ...)
}

# some plots have randomized output
set.seed(101)
