library(shinytest2)

test_that("Migrated shinytest test: mytest.R", {
  app <- AppDriver$new(variant=platform_variant(r_version=FALSE))

  app$expect_values()
  app$expect_screenshot()
})
