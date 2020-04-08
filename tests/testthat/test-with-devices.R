context("with_device")

skip_on_cran()
skip_if_not_installed("shinytest")
skip_if_not_installed("ggplot2")

# a la shinycoreci:::platform()
shinytest_suffix <- function() {
  if (.Platform$OS.type == "windows") {
    return("win")
  }
  sys <- Sys.info()[["sysname"]]
  if (sys == "Darwin") {
    return("mac")
  }
  if (sys == "Linux") {
    return("linux")
  }
  stop("unknown platform")
}

expect_app_doppelganger <- function(appDir) {
  shinytest::expect_pass(shinytest::testApp(appDir, suffix = shinytest_suffix()))
}


test_that("Custom fonts with ragg", {
  skip_if_not_installed("ragg")
  expect_app_doppelganger("agg_png")
})


# The remaining tests rely on showtext to render custom fonts
skip_if_not_installed("showtext")

test_that("Custom fonts with Cairo package", {
  skip_if_not_installed("Cairo")
  expect_app_doppelganger("CairoPNG")
})

test_that("Custom fonts with quartz device", {
  skip_if_not(capabilities()[["aqua"]])
  expect_app_doppelganger("quartz_png")
})

# Remaining tests are rmarkdown specific
skip_if_not_installed("rmarkdown")

test_that("Can render non-custom fonts in rmarkdown with quartz png", {
  skip_if_not(capabilities()[["aqua"]])

  expect_app_doppelganger("quartz_png_rmd")
})

test_that("Can render non-custom fonts in rmarkdown with CairoPNG", {
  skip_if_not_installed("Cairo")

  expect_app_doppelganger("CairoPNG_rmd")
})


test_that("pdf_document compiles without error", {
  outfile <- rmarkdown::render("pdf.Rmd", quiet = TRUE)
  expect_true(file.exists(outfile))
  unlink(outfile)
})
