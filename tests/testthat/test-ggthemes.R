context("ggthemes")

skip_if_not_installed("ggplot2")

library(ggplot2)

test_that("works as expected with global ggthemes", {
  ggtheme <- theme_get()
  on.exit(theme_set(ggtheme), add = TRUE)
  theme_set(theme_void())
  thematic_on("black", "white", accent = NA, font = font_spec(scale = 1.5))
  expect_doppelganger("ggtheme-void", qplot(1:10))
  theme_set(theme_minimal())
  expect_doppelganger("ggtheme-minimal", qplot(1:10))
  thematic_off()
  expect_doppelganger("ggtheme-minimal-off", qplot(1:10))
})


test_that("works as expected with plot-specific themes", {
  thematic_on(bg = "#222222", fg = "white", accent = NA)
  on.exit(thematic_off(), add = TRUE)
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    facet_wrap(~cyl)
  p <- p + theme(strip.background = element_rect(fill = "purple"))
  expect_doppelganger("purple-strip", p)

  p <- qplot(1:10) + theme(
    axis.text = element_text(colour = "red", size = 5),
    axis.text.x = element_text(size = 50)
  )
  expect_doppelganger("axis-interitance", p)

  p <- qplot(1:10) + theme(
    text = element_text(colour = "red", size = 5),
    axis.text.x.bottom = element_text(size = 50)
  )
  expect_doppelganger("axis-interitance-distant", p)
  expect_doppelganger("axis-interitance-distant2", last_plot())
})


test_that("Global ggthemes are respected", {

  p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color =  Species)) +
    geom_point()

  with_ggtheme <- function(theme, expr) {
    old_theme <- theme_set(theme)
    on.exit(theme_set(old_theme), add = TRUE)
    thematic_with_theme(thematic_theme("black", "white", NA), expr)
  }

  # ggplot2 themes
  expect_doppelganger("bw", function() with_ggtheme(theme_bw(), p))
  expect_doppelganger("dark", function() with_ggtheme(theme_dark(), p))
  expect_doppelganger("minimal", function() with_ggtheme(theme_minimal(), p))
  expect_doppelganger("classic", function() with_ggtheme(theme_classic(), p))
  expect_doppelganger("light", function() with_ggtheme(theme_light(), p))
  expect_doppelganger("linedraw", function() with_ggtheme(theme_linedraw(), p))
  expect_doppelganger("void", function() with_ggtheme(theme_void(), p))

  # ggthemes
  skip_if_not_installed("ggthemes")
  ggthemes <- asNamespace("ggthemes")
  expect_doppelganger("wsj", function() with_ggtheme(ggthemes$theme_wsj(), p))
  expect_doppelganger("base", function() with_ggtheme(ggthemes$theme_base(), p))
  expect_doppelganger("solid", function() with_ggtheme(ggthemes$theme_solid(), p))
  expect_doppelganger("solarized", function() with_ggtheme(ggthemes$theme_solarized(), p))
  expect_doppelganger("economist", function() with_ggtheme(ggthemes$theme_economist(), p))
  expect_doppelganger("calc", function() with_ggtheme(ggthemes$theme_calc(), p))
  expect_doppelganger("fivethirtyeight", function() with_ggtheme(ggthemes$theme_fivethirtyeight(), p))
  expect_doppelganger("map", function() with_ggtheme(ggthemes$theme_map(), p))
  expect_doppelganger("excel", function() with_ggtheme(ggthemes$theme_excel(), p))
  expect_doppelganger("clean", function() with_ggtheme(ggthemes$theme_clean(), p))
  expect_doppelganger("excel_new", function() with_ggtheme(ggthemes$theme_excel_new(), p))
  expect_doppelganger("igray", function() with_ggtheme(ggthemes$theme_igray(), p))
  expect_doppelganger("foundation", function() with_ggtheme(ggthemes$theme_foundation(), p))
  expect_doppelganger("few", function() with_ggtheme(ggthemes$theme_few(), p))
  expect_doppelganger("pander", function() with_ggtheme(ggthemes$theme_pander(), p))
  expect_doppelganger("solarized_2", function() with_ggtheme(ggthemes$theme_solarized_2(), p))
  expect_doppelganger("gdocs", function() with_ggtheme(ggthemes$theme_gdocs(), p))
  expect_doppelganger("tufte", function() with_ggtheme(ggthemes$theme_tufte(), p))
  expect_doppelganger("par", function() with_ggtheme(ggthemes$theme_par(), p))
  expect_doppelganger("stata", function() with_ggtheme(ggthemes$theme_stata(), p))
  expect_doppelganger("hc", function() with_ggtheme(ggthemes$theme_hc(), p))
  skip_if_not_installed("dplyr")
  expect_doppelganger("economist_white", function() with_ggtheme(ggthemes$theme_economist_white(), p))

})
