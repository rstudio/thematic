context("rmarkdown")

test_that("Can render non-custom fonts in rmarkdown", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("shinytest")
  skip_if_not_installed("showtext")
  skip_if_not(capabilities("aqua"))

  shinytest::expect_pass(shinytest::testApp("rmarkdown"))
})
