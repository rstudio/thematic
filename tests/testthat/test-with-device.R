context("with_device")

test_that("Can render non-custom fonts on cross-platform devices", {
  skip_on_cran()
  skip_if_not(capabilities()[["cairo"]])
  skip_if_not_installed("Cairo")
  skip_if_not_installed("shinytest")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ragg")
  skip_if_not_installed("showtext")

  shinytest::expect_pass(shinytest::testApp("with_device/cross_platform"))
})


test_that("Can render non-custom fonts on quartz device", {
  skip_on_cran()
  skip_if_not(capabilities()[["aqua"]])
  skip_if_not_installed("shinytest")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("showtext")

  shinytest::expect_pass(shinytest::testApp("with_device/quartz"))
})

# TODO: test for win.metafile?
