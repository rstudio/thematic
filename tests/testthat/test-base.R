context("base")


test_that("base baselines", {
  thematic_local_theme(
    thematic_theme("black", "white", "violet", font_spec("Amatic SC", 1.5, update = TRUE))
  )

  expect_doppelganger("scatter", function() { plot(1:10) })
  expect_doppelganger("scatter-cols", function() { plot(1:10, col = 1:10) })
  expect_doppelganger("lines", function() {
    plot(1:10, (1:10)^2, type = "b", pch = 19, xlab = "x", ylab = "y")
  })
  expect_doppelganger("lines-accent", function() {
    plot(1:10, (1:10)^2, type = "b", pch = 19, xlab = "x", ylab = "y", col = thematic_get_option("accent"))
  })
  expect_doppelganger("lines-cols", function() {
    plot(1:10, (1:10)^2, type = "b", pch = 19, xlab = "x", ylab = "y", col = 1:10)
  })
  expect_doppelganger("smooth-accent", function() {
    # draw a smooth line through a scatter plot
    plot(cars, main = "Stopping Distance versus Speed")
    lines(stats::lowess(cars), col = thematic_get_option("accent"))
  })
  expect_doppelganger("hist", function() { hist(rnorm(100)) })
  expect_doppelganger("barplot", function() {
    barplot(VADeaths[1:3, "Rural Male"])
  })
  expect_doppelganger("barplot-accent", function() {
    barplot(VADeaths[1:3, "Rural Male"], col = thematic_get_option("accent"))
  })
  expect_doppelganger("stripchart", function() {
    stripchart(len ~ dose, data = ToothGrowth,
               pch = 19, frame = FALSE, vertical = TRUE,
               method = "jitter")
  })
  expect_doppelganger("stripchart-cols", function() {
    stripchart(len ~ dose, data = ToothGrowth,
               pch = 19, col = thematic_get_option("qualitative"),
               method = "jitter")
  })

  expect_doppelganger("pies", function() {
    df <- data.frame(
      group = c("Male", "Female", "Child"),
      value = c(25, 25, 50)
    )
    par(mfrow = c(2, 1))
    pie(df$value, labels = df$group, radius = 1)
    pie(df$value, labels = df$group, radius = 1, col = thematic_get_option("qualitative"))
  })

  expect_doppelganger("qq", function() {
    stats::qqnorm(ToothGrowth$len, pch = 1, frame = FALSE)
    stats::qqline(ToothGrowth$len, col = thematic_get_option("accent"), lwd = 2)
  })

  expect_doppelganger(
    "pairs",
    function() {
      pairs(
        iris[,1:4], pch = 19, cex = 0.5,
        col = thematic_get_option("qualitative")[iris$Species],
        lower.panel = NULL
      )
    }
  )

})

