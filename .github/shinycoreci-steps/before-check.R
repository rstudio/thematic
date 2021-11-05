# Install testing dependencies
tinytex::install_tinytex()


# Only test vdiffr & shinytest on `release` R version
current_r_version <- base::numeric_version(paste0(R.version$major, ".", R.version$minor))
latest_r_version <- base::numeric_version(rversions::r_release()$version[1])
if (current_r_version == latest_r_version) {
  # Test vdiffr
  withr::with_envvar(
    list(
      VDIFFR_RUN_TESTS = "true",
      VDIFFR_LOG_PATH = "../vdiffr.Rout.fail",
      SHINYTEST_RUN_TESTS = "true"
    ),
    {
      # Run test() before R CMD check since, for some reason, rcmdcheck::rcmdcheck() skips vdiffr tests
      devtools::install()
      res <- devtools::test()
      df <- as.data.frame(res)
      if (sum(df$failed) > 0 || any(df$error)) {
        stop("GHA CI tests failed")
      }
    }
  )
}
