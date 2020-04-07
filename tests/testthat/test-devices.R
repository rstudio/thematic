context("with_device")

skip_on_cran()
skip_if_not_installed("shinytest")
skip_if_not_installed("ggplot2")


test_that("Custom fonts with ragg", {
  skip_if_not_installed("ragg")
  shinytest::expect_pass(shinytest::testApp("agg_png"))
})


# The remaining tests rely on showtext to render custom fonts
skip_if_not_installed("showtext")

test_that("Custom fonts with Cairo package", {
  skip_if_not_installed("Cairo")

  shinytest::expect_pass(shinytest::testApp("CairoPNG"))
})

test_that("Custom fonts with quartz device", {
  skip_if_not(capabilities()[["aqua"]])

  shinytest::expect_pass(shinytest::testApp("quartz_png"))
})

test_that("Custom fonts with cairo capabilities", {
  skip_if_not(capabilities()[["cairo"]])

  shinytest::expect_pass(shinytest::testApp("cairo_png"))
  shinytest::expect_pass(shinytest::testApp("cairo_svg"))
})

# Remaining tests are rmarkdown specific
skip_if_not_installed("rmarkdown")

test_that("Can render non-custom fonts in rmarkdown with quartz png", {
  skip_if_not(capabilities()[["aqua"]])

  shinytest::expect_pass(shinytest::testApp("quartz_png_rmd"))
})

test_that("Can render non-custom fonts in rmarkdown with quartz png", {
  skip_if_not(capabilities()[["aqua"]])

  shinytest::expect_pass(shinytest::testApp("CairoPNG_rmd"))
})

