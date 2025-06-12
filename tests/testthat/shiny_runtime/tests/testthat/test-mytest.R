library(shinytest2)

test_that("Migrated shinytest test: mytest.R", {
  app <- AppDriver$new("../../runtime.Rmd", seed = 46815)

  app$expect_values()
  app$expect_screenshot()
})
