context("rmarkdown")

test_that("Can render non-custom fonts in rmarkdown", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("shinytest")
  skip_if_not_installed("showtext")

  # All the Rmds under this path are tests, except for the template
  rmd_dir <- testthat::test_path("rmarkdown")
  rmds <- setdiff(dir(rmd_dir, pattern = "\\.Rmd"), "template.Rmd")

  for (rmd in rmds) {
    shinytest::expect_pass(
      shinytest::testApp(file.path("rmarkdown", rmd))
    )
  }
})
