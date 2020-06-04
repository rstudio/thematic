context("auto-config")

test_that("Can influence auto resolution with config", {
  old_config <- auto_config_set(auto_config("black", "white", NA, font = "Pacifico"))
  on.exit(auto_config_set(old_config), add = TRUE)

  theme <- thematic_theme(font = "auto")
  expect_true(theme$bg == "auto")
  expect_true(theme$fg == "auto")
  expect_true(theme$accent == "auto")
  theme <- auto_resolve_theme(theme)
  expect_true(theme$bg == "black")
  expect_true(theme$fg == "white")
  expect_true(is.na(theme$accent))
  expect_true(theme$font$families == "Pacifico")
})

# TODO: add a test(s) for different priority settings?
