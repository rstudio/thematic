context("shinytest")

skip_on_cran()
skip_if_not_installed("shinytest")
skip_if_not_installed("ggplot2")
skip_if_not_installed("lattice")
# Run tests on release version of R
skip_if_not(as.logical(Sys.getenv("SHINYTEST_RUN_TESTS", "true")))


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

# TODO: test for auto theming in shiny with ragg?

# The remaining tests rely on showtext to render custom fonts
skip_if_not_installed("showtext")

test_that("Auto theming in shiny works", {
  # These shiny apps currently use remote urls to import Google fonts
  skip_if_offline()
  skip_if_not(capabilities()[["aqua"]])
  expect_app_doppelganger("auto_theme_shiny/base")
  expect_app_doppelganger("auto_theme_shiny/lattice")
  expect_app_doppelganger("auto_theme_shiny/ggplot2")
  expect_app_doppelganger("auto_theme_shiny/local")
  expect_app_doppelganger("shiny_runtime")
})

test_that("Custom fonts with Cairo package", {
  skip_if_not_installed("Cairo")
  expect_app_doppelganger("CairoPNG")
})

test_that("Custom fonts with quartz device", {
  skip_if_not(capabilities()[["aqua"]])
  expect_app_doppelganger("quartz_png")
})

test_that("Custom fonts with cairo capabilities", {
  skip_if_not(capabilities()[["cairo"]])
  expect_app_doppelganger("cairo_png")
  # TODO: figure out why this is failing on mac
  skip_if_not(!identical(shinytest_suffix(), "mac"))
  expect_app_doppelganger("cairo_svg")
})

# Remaining tests are rmarkdown specific
skip_if_not_installed("rmarkdown")

test_that("Auto theming in rmarkdown works", {
  skip_if_not(capabilities()[["aqua"]])
  expect_app_doppelganger("auto_theme_rmd/darkly_rmd")
})

test_that("Can render non-custom fonts in rmarkdown with quartz png", {
  skip_if_not(capabilities()[["aqua"]])
  expect_app_doppelganger("quartz_png_rmd")
})

test_that("Can render non-custom fonts in rmarkdown with CairoPNG", {
  skip_if_not_installed("Cairo")
  expect_app_doppelganger("CairoPNG_rmd")
})

test_that("pdf_document compiles without error", {
  skip_if_not(.Platform$OS.type != "windows")
  outfile <- rmarkdown::render("pdf.Rmd", quiet = TRUE)
  expect_true(file.exists(outfile))
  unlink(outfile)
})


