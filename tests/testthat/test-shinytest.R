skip_on_cran()
skip_if_not_installed("shinytest2")
skip_if_not_installed("callr")
skip_if_not_installed("ggplot2")
skip_if_not_installed("lattice")


test_app <- function(appDir) {
  getFromNamespace("test_app", "shinytest2")(appDir, check_setup = FALSE)
}

platform <- getFromNamespace("platform_variant", "shinytest2")(
  r_version = FALSE
)


# Default to running tests on macOS
do_tests <- identical(platform, "mac")
skip_if_not(as.logical(Sys.getenv("SHINYTEST_RUN_TESTS", do_tests)))


test_that("Custom fonts with ragg", {
  skip_if_not_installed("ragg")
  test_app("agg_png")
})

test_that("runtime: shiny auto-theming", {
  skip_if_not(identical("win", platform))
  test_app("shiny_runtime")
})

# The remaining tests rely on showtext to render custom fonts
skip_if_not_installed("showtext")

test_that("Auto theming in shiny works", {
  # These shiny apps currently use remote urls to import Google fonts
  skip_if_offline()
  skip_if_not(capabilities()[["aqua"]])
  test_app("auto_theme_shiny/base")
  test_app("auto_theme_shiny/lattice")
  test_app("auto_theme_shiny/ggplot2")
  test_app("auto_theme_shiny/local_theme")
  test_app("auto_theme_shiny/with_theme")
})

test_that("Custom fonts with Cairo package", {
  skip_if_not_installed("Cairo")
  test_app("CairoPNG")
})

test_that("Custom fonts with quartz device", {
  skip_if_not(capabilities()[["aqua"]])
  test_app("quartz_png")
})

test_that("Custom fonts with cairo capabilities", {
  skip_if_not(capabilities()[["cairo"]])
  test_app("cairo_png")
  # TODO: figure out why this is failing on mac
  skip_if_not(!identical(platform, "mac"))
  test_app("cairo_svg")
})

# Remaining tests are rmarkdown specific
skip_if_not_installed("rmarkdown")

test_that("Auto theming in rmarkdown works", {
  skip_if_not(capabilities()[["aqua"]])
  test_app("auto_theme_rmd/darkly_rmd")
})

test_that("Can render non-custom fonts in rmarkdown with quartz png", {
  skip_if_not(capabilities()[["aqua"]])
  test_app("quartz_png_rmd")
})

test_that("Can render non-custom fonts in rmarkdown with CairoPNG", {
  skip_if_not_installed("Cairo")
  test_app("CairoPNG_rmd")
})

test_that("pdf_document compiles without error", {
  skip_if_not(!identical("win", platform))


  outfile <- rmarkdown::render("pdf.Rmd", quiet = TRUE)
  expect_true(file.exists(outfile))
  unlink(outfile)
})
