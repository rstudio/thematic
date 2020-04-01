context("with_device")

test_that("Can render non-custom fonts on cross-platform devices", {
  skip_on_cran()
  skip_if_not_installed("shinytest")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ragg")
  skip_if_not_installed("showtext")
  skip_if_not_installed("Cairo")

  shinytest::expect_pass(shinytest::testApp("with_device/cross_platform"))
})

# Not entirely sure why, but this is reporting a different when NOT_CRAN="true"
# (which is the default of devtools::check()?)
not_cran <- Sys.getenv("NOT_CRAN")
Sys.unsetenv("NOT_CRAN")
test_that("Can render non-custom fonts on quartz device", {
  skip_on_cran()
  skip_if_not_installed("shinytest")
  skip_if_not_installed("ggplot2")
  skip_if_not(capabilities("aqua"))
  skip_if_not_installed("showtext")

  shinytest::expect_pass(shinytest::testApp("with_device/quartz"))
})
Sys.setenv("NOT_CRAN" = not_cran)

# TODO: test for win.metafile?
