context("fonts")


#expect_that("Google Fonts with base", {
#  skip_if_offline()
#  expect_warning("not a font", plot(1:10))
#  expect_doppelganger("pacifico-base", function() plot(1:10))
#})
#
#expect_that("Google Fonts with lattice", {
#  skip_if_offline()
#  expect_doppelganger("pacifico-lattice", lattice::show.settings())
#})

#expect_that("Google Fonts with ggplot2", {
#  skip_if_offline()
#  skip_if_not_installed("ggplot2")
#
#  library(ggplot2)
#  font <- font_spec(c("Definitely not a font", "Pacifico"), scale = 1.25, auto_install = TRUE)
#  thematic_begin(font = font)
#  p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) + geom_text()
#  expect_doppelganger("pacifico-ggplot2", p)
#  thematic_end()
#})



