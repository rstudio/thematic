skip_if_not_installed("ggplot2")
library(ggplot2)

theme <- thematic_theme(
  "#444444", "#e4e4e4", "#749886",
  font = font_spec("Oxanium", scale = 1.25, update = TRUE)
)

test_that("ggplot baselines", {
  thematic_local_theme(theme)

  ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))
  values <- data.frame(
    id = ids,
    value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
  )
  positions <- data.frame(
    id = rep(ids, each = 4),
    x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
          0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
    y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
          2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
  )
  huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))

  dsample <- diamonds[sample(nrow(diamonds), 1000), ]


  expect_doppelganger("GeomPointColor", ggplot(mtcars, aes(wt, mpg, color = cyl)) + geom_point())
  expect_doppelganger("GeomAbline", {
    ggplot(mtcars, aes(mpg, wt)) +
      geom_point() +
      geom_hline(aes(yintercept = wt), data.frame(cyl = c(4, 6, 8), wt = c(2.28, 3.11, 4.00))) +
      facet_wrap(~ cyl)
  })
  expect_doppelganger("GeomBar", ggplot(mpg, aes(class)) + geom_bar(aes(weight = displ)))
  expect_warning(expect_doppelganger("GeomBin2d", ggplot(diamonds, aes(x, y)) + xlim(4, 10) + ylim(4, 10) + geom_bin2d()))
  expect_doppelganger("GeomBox", ggplot(mpg, aes(class, hwy)) + geom_boxplot())
  # For some reason these give slightly different svgs on different platforms?
  #expect_doppelganger("GeomContour", ggplot(faithfuld, aes(waiting, eruptions, z = density)) + geom_contour())
  #expect_doppelganger("GeomContourRaster", ggplot(faithfuld, aes(waiting, eruptions, z = density)) + geom_raster(aes(fill = density)) + geom_contour())
  expect_doppelganger("GeomCount", ggplot(mpg, aes(cty, hwy)) + geom_count())
  expect_doppelganger("GeomDensity", ggplot(diamonds, aes(carat)) + geom_density())
  expect_doppelganger("GeomDensityColor", ggplot(diamonds, aes(depth, fill = cut, color = cut)) + geom_density(alpha = 0.1))
  expect_doppelganger("GeomDotPlot",  ggplot(mtcars, aes(x = mpg)) + geom_dotplot())
  expect_doppelganger(
    "GeomError", {
      ggplot(data.frame(
        trt = factor(c(1, 1, 2, 2)),
        resp = c(1, 5, 3, 4),
        group = factor(c(1, 2, 1, 2)),
        se = c(0.1, 0.3, 0.3, 0.2)
      ), aes(resp, trt, color = group)) + geom_point() +
        geom_errorbarh(aes(xmax = resp + se, xmin = resp - se))
    })
  expect_doppelganger("GeomHistogram",  ggplot(diamonds, aes(price, fill = cut)) + geom_histogram(binwidth = 500))
  expect_doppelganger("GeomJitter", ggplot(mpg, aes(cyl, hwy)) + geom_jitter(width = 0.25))
  expect_doppelganger(
    "GeomErrorbar",  {
      ggplot(data.frame(
        trt = factor(c(1, 1, 2, 2)),
        resp = c(1, 5, 3, 4),
        group = factor(c(1, 2, 1, 2)),
        upper = c(1.1, 5.3, 3.3, 4.2),
        lower = c(0.8, 4.6, 2.4, 3.6)
      ), aes(trt, resp, fill = group)) +
        geom_col(position = "dodge") +
        geom_errorbar(aes(ymin = lower, ymax = upper), position = "dodge", width = 0.25)
    })
  expect_doppelganger(
    "GeomPolygon", {
      ggplot(values) +
        geom_map(aes(map_id = id), map = positions) +
        expand_limits(positions)
    }
  )
  expect_doppelganger(
    "GeomPolygon2", {
      ggplot(values, aes(fill = value)) +
        geom_map(aes(map_id = id), map = positions) +
        expand_limits(positions)
    }
  )
  expect_doppelganger("GeomLine", ggplot(economics_long, aes(date, value01, group = variable)) + geom_line())
  expect_doppelganger("GeomLine2", ggplot(economics_long, aes(date, value01, color = variable)) + geom_line())
  expect_doppelganger("GeomPoint", ggplot(dsample, aes(carat, price)) + geom_point(alpha = 0.1))
  expect_doppelganger("GeomPoint2", ggplot(mtcars, aes(wt, mpg, color = factor(cyl), shape = factor(cyl))) + geom_point())
  expect_doppelganger("GeomRibbon", {
    ggplot(huron, aes(year)) +
      geom_ribbon(aes(ymin = level - 1, ymax = level + 1), fill = "grey70") +
      geom_line(aes(y = level))
  })
  expect_doppelganger("GeomRug", ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_rug())
  expect_doppelganger("GeomRug2", ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_rug(outside = TRUE) + coord_cartesian(clip = "off"))
  expect_doppelganger(
    "GeomCurve", {
      df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
      ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2, color = "curve"), data = df) +
        geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, color = "segment"), data = df) +
        geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2), data = df, curvature = -0.2)
    }
  )

  expect_doppelganger(
    "GeomSmooth", {
      ggplot(dsample, aes(carat, price)) +
        geom_point(alpha = 0.2) +
        geom_smooth() +
        facet_wrap(~cut) + ggtitle("Diamond price by carat and cut")
    })
  expect_doppelganger(
    "GeomSmooth2", {
      ggplot(mpg, aes(displ, hwy, color = class)) +
        geom_point() +
        geom_smooth(se = FALSE, method = lm)
    }
  )
  expect_doppelganger(
    "GeomSpoke", {
      df <- expand.grid(x = 1:10, y=1:10)
      df$angle <- runif(100, 0, 2*pi)
      df$speed <- runif(100, 0, sqrt(0.1 * df$x))

      ggplot(df, aes(x, y)) +
        geom_point() +
        geom_spoke(aes(angle = angle), radius = 0.5)
    }
  )
  expect_doppelganger("GeomText", ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) + geom_text())
  expect_doppelganger("GeomLabel", ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) + geom_label())
  expect_doppelganger("GeomTile", {
    ggplot(data.frame(
      x = rep(c(2, 5, 7, 9, 12), 2),
      y = rep(c(1, 2), each = 5),
      z = factor(rep(1:5, each = 2)),
      w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
    ), aes(x, y, width = w)) + geom_tile(aes(fill = z))
  })
  expect_doppelganger("GeomViolin", ggplot(mtcars, aes(factor(cyl), mpg)) + geom_violin())
})

test_that("Scale defaults can be overridden", {
  thematic_local_theme(theme)

  expect_doppelganger("sequential-color", {
    ggplot(mtcars, aes(wt, mpg, color = cyl)) +
      geom_point() +
      scale_color_gradient2(midpoint = 6)
  })

  expect_doppelganger("sequential-fill", {
    ggplot(faithfuld, aes(waiting, eruptions, z = density)) +
      geom_raster(aes(fill = density)) +
      scale_fill_gradient2(midpoint = 0.02)
  })

  expect_doppelganger("qualitative-color", {
    ggplot(economics_long) +
      geom_line(aes(date, value01, color = variable)) +
      scale_color_brewer(palette = "Accent")
  })

  expect_doppelganger("qualitative-fill", {
    ggplot(mtcars, aes(mpg, fill = factor(am))) +
      geom_density() +
      scale_fill_brewer(palette = "Accent")
  })

})

test_that("sf integration", {
  skip_if_not_installed("sf")
  skip_if_not_installed("rgdal")
  skip_if_not(Sys.info()[["sysname"]] == "Darwin")
  st_read <- getFromNamespace("st_read", "sf")
  st_transform <- getFromNamespace("st_transform", "sf")
  st_centroid <- getFromNamespace("st_centroid", "sf")

  thematic_local_theme(theme)

  nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  nc_3857 <- st_transform(nc, 3857)
  nc_3857$mid <- st_centroid(nc_3857$geometry)

  p <- ggplot(nc_3857) +
    geom_sf(color = "white") +
    geom_sf(aes(geometry = mid, size = AREA), show.legend = "point")

  expect_doppelganger("GeomSf", p)
})

theme2 <- thematic_theme(
  bg = "black", fg = "white", accent = "salmon",
  font_spec("Oxanium", scale = 1.25)
)

test_that("gridExtra integration", {
  skip_if_not_installed("gridExtra")

  thematic_local_theme(theme2)
  p1 <- qplot(x = 1:10, y = 1:10, color = 1:10)
  p2 <- qplot(x = 1:10, y = 1:10, color = 1:10)
  expect_doppelganger(
    "grid-arrange", getFromNamespace("grid.arrange", "gridExtra")(p1, p2)
  )
})

test_that("patchwork integration", {
  skip_if_not_installed("patchwork")
  ggplot2 <- "patchwork"
  library(ggplot2)
  p1 <- qplot(x = 1:10, y = 1:10, color = 1:10)
  p2 <- qplot(x = 1:10, y = 1:10, color = 1:10)
  thematic_local_theme(theme2)
  expect_doppelganger("patchwork", p1 + p2)
})

# https://github.com/rstudio/thematic/pull/83
test_that("element_blank() inherits properly", {
  thematic_local_theme(theme2)
  expect_doppelganger(
    "no-y-text",
    qplot(x = 1:10, y = 1:10) + theme(axis.text.y = element_blank())
  )
})


test_that("Relevant ggplot_build() method(s) are owned by thematic", {
  ggplot_uses_S7 <- "class_ggplot" %in% getNamespaceExports("ggplot2")
  skip_if(ggplot_uses_S7)
  thematic_local_theme(theme2)
  expect_equal(
    body(getFromNamespace("ggplot_build.ggplot", "ggplot2")),
    body(ggthematic_build)
  )
  skip_if_not_installed("gganimate")
  expect_equal(
    body(getFromNamespace("ggplot_build.gganim", "gganimate")),
    body(ggthematic_build)
  )
})
