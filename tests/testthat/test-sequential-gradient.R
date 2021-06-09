context("sequential_gradient")

skip_if_not_installed("ggplot2")

test_that("Sequential gradients works as expected", {
  library(ggplot2)

  on.exit(thematic_off(), add = TRUE)

  # Gradient from fg to accent
  fg <- sequential_gradient(1, 0)
  thematic_on("black", "white", "salmon", sequential = fg)
  p <- ggplot2::qplot(1:10, 1:10, color = 1:10)
  expect_doppelganger("fg-accent", p)

  # Gradient from accent -> bg
  bg <- sequential_gradient(0, 1)
  thematic_on("black", "white", "salmon", sequential = bg)
  p <- ggplot2::qplot(1:10, 1:10, color = 1:10)
  expect_doppelganger("accent-bg", p)

  # Gradient from mix(accent, fg, 0.5) -> mix(accent, bg, 0.5)
  mix <- sequential_gradient(0.5, 0.5)
  thematic_on("black", "white", "salmon", sequential = mix)
  p <- ggplot2::qplot(1:10, 1:10, color = 1:10)
  expect_doppelganger("half-half", p)

  # Use fg (instead of bg) for high end of scale
  mix_flip <- sequential_gradient(0.5, 0.5, fg_low = FALSE)
  thematic_on("black", "white", "salmon", sequential = mix_flip)
  ggplot2::qplot(1:10, 1:10, color = 1:10)
  expect_doppelganger("half-half-reversed", p)
})
