context("rmarkdown")

test_that("Can render non-custom fonts in rmarkdown", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("shinytest")
  skip_if_not_installed("showtext")

  # Temporarily unset this R CMD check envvar since
  # the way we map device name to a device function _might_
  # lead to an on-screen device
  #screen_device <- Sys.getenv("_R_CHECK_SCREEN_DEVICE_")
  #Sys.setenv("_R_CHECK_SCREEN_DEVICE_" = "")
  shinytest::expect_pass(shinytest::testApp("rmarkdown"))
  #Sys.setenv("_R_CHECK_SCREEN_DEVICE_" = screen_device)
})
