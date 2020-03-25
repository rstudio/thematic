context("google-fonts")

test_that("gfont id is correctly constructed from family name", {
  expect_true(
    all(google_fonts$id == gfont_id(google_fonts$family))
  )
  skip_if_offline()
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("curl")
  google_fonts2 <- try_fetch_google_fonts()
  expect_true(
    all(google_fonts2$id == gfont_id(google_fonts2$family))
  )
})
