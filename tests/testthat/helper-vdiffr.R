# vdiffr ignores failures when
#   - VDIFFR_RUN_TESTS is "false" (on Travis CI with older versions and dev version of R)
#   - CI is not set (on CRAN)


# Note that the default vdiffr::write_svg specifies a weird
# default theme for ggplot2's testing purposes
write_svg_base <- function(plot, file, title = "") {
  thematic_with_device(
    plot, device = grDevices::svg,
    filename = file, width = 7, height = 7
  )
}

expect_doppelganger <- function(name, p, ...) {
  vdiffr::expect_doppelganger(name, p, ..., writer = write_svg_base)
}

# some plots have randomized output
set.seed(101)
