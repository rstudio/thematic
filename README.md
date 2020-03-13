
<!-- README.md is generated from README.Rmd. Please edit that file -->

# thematic

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/thematic)](https://CRAN.R-project.org/package=thematic)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/rstudio/thematic.svg?branch=master)](https://travis-ci.org/rstudio/thematic)
[![Codecov test
coverage](https://codecov.io/gh/rstudio/thematic/branch/master/graph/badge.svg)](https://codecov.io/gh/rstudio/thematic?branch=master)
<!-- badges: end -->

Simple and automatic theming of **ggplot2**, **lattice**, and **base**
graphics.

## Installation

**thematic** is not yet available on [CRAN](https://CRAN.R-project.org),
but you can install it now with:

``` r
remotes::install_github("cpsievert/thematic")
library(thematic)
```

## Getting started

### Main colors

``` r
library(ggplot2)
p <- ggplot(faithfuld, aes(waiting, eruptions, z = density)) +
  geom_raster(aes(fill = density)) + 
  geom_contour()
```

``` r
thematic_begin(bg = "darkblue", fg = "skyblue", accent = "orange")
p
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="70%" style="display: block; margin: auto;" />

``` r
thematic_begin("#444444", "#e4e4e4", "#749886")
p
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="70%" style="display: block; margin: auto;" />

``` r
lattice::show.settings()
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="70%" style="display: block; margin: auto;" />

``` r
image(volcano)
image(volcano, col = thematic_current("sequential"))
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="70%" style="display: block; margin: auto;" /><img src="man/figures/README-unnamed-chunk-6-2.png" width="70%" style="display: block; margin: auto;" />

### Fonts

There are two main controls for fonts currently: `family` and `scale`.
`scale` (defaults to 1) is multiplied against all relevant font sizes.
If `family` references a font that doesn’t exist on the system, but the
font is available on [Google Fonts](https://fonts.google.com/),
**thematic** will try to download and register the font files for you
(as long as `auto_install = TRUE`).

``` r
font <- font_spec(family = "Oxanium", scale = 1.25, auto_install = TRUE)
thematic_begin("black", "white", font = font)
p
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="70%" style="display: block; margin: auto;" />

``` r
thematic_end()
p
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="70%" style="display: block; margin: auto;" />

## With shiny auto-theming

**shiny** uses **thematic**’s functionality to implement it’s
auto-theming feature.

TODO: an example of overriding auto-theming defaults

## Known issues

  - Auto-installed fonts currently don’t work at all with the RStudio
    graphics device. They should generally work with other graphics
    devices (i.e., **rmarkdown**/**knitr**) *if* you have the
    **showtext** package installed. In the case you don’t want to take a
    dependency on **showtext**, you can use `thematic_with_device()`,
    which will (with `device = safe_device()`), which

<!-- end list -->

``` r
font <- font_spec(family = "Caveat", scale = 1.25)
thematic_begin("black", "white", font = font)
file <- thematic_with_device(plot(1), res = 144)
```

If you’re in RStudio, you can preview the resulting `file` with
`file.show(file)`. Moreover, to embed the `file` in
**rmarkdown**/**knitr**, do `knitr::include_graphics(file)`.
