context("lattice")

skip_if_not_installed("lattice")
library(lattice)
library(stats)

theme <-  thematic_theme(
  "black", "white", "pink",
  font = font_spec("Rock Salt", update = TRUE)
)

test_that("lattice baselines", {
  thematic_local_theme(theme)

  expect_doppelganger("dot_plot", dotplot(variety ~ yield | year * site, data=barley))

  expect_doppelganger("level_plot", {
      x <- seq(pi/4, 5 * pi, length.out = 100)
      y <- seq(pi/4, 5 * pi, length.out = 100)
      r <- as.vector(sqrt(outer(x^2, y^2, "+")))
      grid <- expand.grid(x=x, y=y)
      grid$z <- cos(r^2) * exp(-r/(pi^3))
      levelplot(z ~ x * y, grid, cuts = 50, scales=list(log="e"), xlab="",
                ylab="", main="Weird Function", sub="with log scales",
                colorkey = FALSE, region = TRUE)
    }
  )
  expect_doppelganger("density_plot", {
      histogram( ~ height | voice.part, data = singer,
                 xlab = "Height (inches)", type = "density",
                 panel = function(x, ...) {
                   panel.histogram(x, ...)
                   panel.mathdensity(dmath = dnorm, col = "black",
                                     args = list(mean=mean(x),sd=sd(x)))
                 } )
  }
  )

  expect_doppelganger("cleveland", {
      EE <- equal.count(ethanol$E, number=9, overlap=1/4)

      ## Constructing panel functions on the fly; prepanel
      xyplot(NOx ~ C | EE, data = ethanol,
             prepanel = function(x, y) prepanel.loess(x, y, span = 1),
             xlab = "Compression Ratio", ylab = "NOx (micrograms/J)",
             panel = function(x, y) {
               panel.grid(h = -1, v = 2)
               panel.xyplot(x, y)
               panel.loess(x, y, span=1)
             },
             aspect = "xy")
    })

  expect_doppelganger("xyplot", {
      xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
             data = iris, scales = "free", layout = c(2, 2),
             auto.key = list(x = .6, y = .7, corner = c(0, 0)))
    })

  expect_doppelganger("interaction_plot", {
      xyplot(decrease ~ treatment, OrchardSprays, groups = rowpos,
             type = "a",
             auto.key =
               list(space = "right", points = FALSE, lines = TRUE))
      })

  expect_doppelganger("bwplot", {
      bwplot(decrease ~ treatment, OrchardSprays, groups = rowpos,
             panel = "panel.superpose",
             panel.groups = "panel.linejoin",
             xlab = "treatment",
             key = list(lines = Rows(trellis.par.get("superpose.line"),
                                     c(1:7, 1)),
                        text = list(lab = as.character(unique(OrchardSprays$rowpos))),
                        columns = 4, title = "Row position"))
    })

  expect_doppelganger("settings", show.settings())


  thematic_local_theme(thematic_theme("black", "white", c("orange", "blue")))
  expect_doppelganger("settings2", show.settings())
})

