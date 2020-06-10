context("ggthemes")

test_that("Global ggthemes are respected", {
  library(ggplot2)
  library(ggthemes)

  p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color =  Species)) +
    geom_point()

  with_ggtheme <- function(theme, expr) {
    old_theme <- theme_set(theme)
    on.exit(old_theme, add = TRUE)
    thematic_with_theme(thematic_theme("black", "white"), expr)
  }

  expect_doppelganger("wsj", function() with_ggtheme(theme_wsj(), p))
  expect_doppelganger("economist_white", function() with_ggtheme(theme_economist_white(), p))
  expect_doppelganger("base", function() with_ggtheme(theme_base(), p))
  expect_doppelganger("solid", function() with_ggtheme(theme_solid(), p))
  expect_doppelganger("solarized", function() with_ggtheme(theme_solarized(), p))
  expect_doppelganger("economist", function() with_ggtheme(theme_economist(), p))
  expect_doppelganger("calc", function() with_ggtheme(theme_calc(), p))
  expect_doppelganger("fivethirtyeight", function() with_ggtheme(theme_fivethirtyeight(), p))
  expect_doppelganger("map", function() with_ggtheme(theme_map(), p))
  expect_doppelganger("excel", function() with_ggtheme(theme_excel(), p))
  expect_doppelganger("clean", function() with_ggtheme(theme_clean(), p))
  expect_doppelganger("excel_new", function() with_ggtheme(theme_excel_new(), p))
  expect_doppelganger("igray", function() with_ggtheme(theme_igray(), p))
  expect_doppelganger("foundation", function() with_ggtheme(theme_foundation(), p))
  expect_doppelganger("few", function() with_ggtheme(theme_few(), p))
  expect_doppelganger("pander", function() with_ggtheme(theme_pander(), p))
  expect_doppelganger("solarized_2", function() with_ggtheme(theme_solarized_2(), p))
  expect_doppelganger("gdocs", function() with_ggtheme(theme_gdocs(), p))
  expect_doppelganger("tufte", function() with_ggtheme(theme_tufte(), p))
  expect_doppelganger("par", function() with_ggtheme(theme_par(), p))
  expect_doppelganger("stata", function() with_ggtheme(theme_stata(), p))
  expect_doppelganger("hc", function() with_ggtheme(theme_hc(), p))

})
