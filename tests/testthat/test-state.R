context("state management")

test_that("thematic_with_theme()", {
  expect_null(thematic_get_theme())
  expect_doppelganger("scatter-no-theme", function() { plot(1:10) })
  expect_doppelganger("scatter-with-theme", function() {
    thematic_with_theme(
      thematic_theme("black", "white", NA),
      plot(1:10)
    )
  })
  expect_doppelganger("scatter-no-theme2", function() { plot(1:10) })
})

test_that("Setting and getting the theme", {
  expect_null(thematic_get_theme())
  thematic_on("black", "white", "green")
  x <- thematic_get_theme()
  expect_true(is_thematic_theme(x))
  y <- thematic_on("darkblue", "skyblue", "purple")
  expect_identical(x, y)
  expect_doppelganger("purple", qplot(1:10, 1:10, color = 1:10))
  thematic_set_theme(y)
  expect_equal(thematic_get_theme(), y)
  expect_doppelganger("green", qplot(1:10, 1:10, color = 1:10))
  thematic_set_theme(NULL)
  expect_null(thematic_get_theme())
  expect_doppelganger("none", qplot(1:10, 1:10, color = 1:10))
  thematic_off()
})

test_that("Theme inheritance works", {
  thematic_on("black", "red", "green")
  white <- thematic_theme(fg = "white", inherit = TRUE)
  thematic_on(fg = "white", inherit = TRUE)
  expect_equal(thematic_get_theme(resolve = FALSE), white)
  expect_equal(thematic_get_theme(), auto_resolve_theme(white))
  expect_doppelganger("white-green", qplot(1:10, 1:10, color = 1:10))
  thematic_on(accent = "red", inherit = TRUE)
  expect_doppelganger("white-red", qplot(1:10, 1:10, color = 1:10))
  thematic_off()
})

test_that("Getting options", {
  expect_null(thematic_get_theme())
  expect_equal(thematic_get_option("bg", "purple"), "purple")
  expect_equal(thematic_get_option("fg", "yellow"), "yellow")
  expect_equal(thematic_get_option("accent", "orange"), "orange")

  thematic_on("black", "white", "green")
  expect_equal(thematic_get_option("bg"), "black")
  expect_equal(thematic_get_option("fg"), "white")
  expect_equal(thematic_get_option("accent"), "green")
  expect_equal(
    thematic_get_mixture(seq(0, 1, by = 0.1)),
    c('#000000', '#1B1B1B', '#303030', '#474747', '#5E5E5E', '#777777', '#919191', '#ABABAB', '#C6C6C6', '#E2E2E2', '#FFFFFF')
  )
  thematic_off()
})
