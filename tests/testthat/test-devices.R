context("with_device")

skip_on_cran()
skip_if_not_installed("shinytest")
skip_if_not_installed("ggplot2")


test_that("Custom fonts with ragg", {
  skip_if_not_installed("ragg")
  shinytest::expect_pass(shinytest::testApp("with_device/ragg"))
})

# The remaining tests rely on showtext to render custom fonts
skip_if_not_installed("showtext")

test_that("Custom fonts on quartz device", {
  skip_if_not(capabilities()[["aqua"]])

  shinytest::expect_pass(shinytest::testApp("with_device/quartz"))
})

test_that("Custom fonts on cairo capabilities", {
  skip_if_not(capabilities()[["cairo"]])

  shinytest::expect_pass(shinytest::testApp("with_device/cairo_base"))
})

test_that("Custom fonts on Cairo package", {
  skip_if_not_installed("Cairo")

  shinytest::expect_pass(shinytest::testApp("with_device/Cairo"))
})
