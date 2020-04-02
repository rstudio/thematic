context("rmarkdown")

# Not entirely sure why, but this is reporting a different when NOT_CRAN="true"
# (which is the default of devtools::check()?)
not_cran <- Sys.getenv("NOT_CRAN")
Sys.unsetenv("NOT_CRAN")
test_that("Can render non-custom fonts in rmarkdown", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("shinytest")
  skip_if_not_installed("showtext")

  shinytest::expect_pass(shinytest::testApp("rmarkdown"))
})
Sys.setenv("NOT_CRAN" = not_cran)
