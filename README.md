
<!-- README.md is generated from README.Rmd. Please edit that file -->

# thematic <a href='https://rstudio.github.io/thematic/'><img src='man/figures/logo.png' align="right" height="138.5" style="margin:10px;" /></a>

<!-- badges: start -->

[![R build
status](https://github.com/rstudio/thematic/workflows/R-CMD-check/badge.svg)](https://github.com/rstudio/thematic)
[![CRAN
status](https://www.r-pkg.org/badges/version/thematic)](https://CRAN.R-project.org/package=thematic)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

Simplified theming of **ggplot2**, **lattice**, and **base** R graphics.
In addition to providing a [centralized
approach](https://rstudio.github.io/thematic/articles/custom.html) to
styling R graphics, **thematic** also enables [automatic
styling](https://rstudio.github.io/thematic/articles/auto.html) of R
plots in Shiny, R Markdown, and RStudio.

## Installation

**thematic** is not yet available on [CRAN](https://CRAN.R-project.org),
but you can install it now with:

``` r
remotes::install_github("rstudio/thematic")
library(thematic)
```

For [auto theming in
Shiny](https://rstudio.github.io/thematic/articles/auto.html#shiny),
you’ll need **shiny** v.1.5.0 or higher.

``` r
install.packages("shiny")
```

For [auto theming in R
Markdown](https://rstudio.github.io/thematic/articles/auto.html#rmd),
you’ll currently need an experimental version of **rmarkdown**:

``` r
remotes::install_github("rstudio/rmarkdown#1706")
```

## Overview

**thematic**’s [auto
theming](https://rstudio.github.io/thematic/articles/auto.html) gives R
plots the ability to style themselves inside
[Shiny](https://rstudio.github.io/thematic/articles/auto.html#shiny)
(via CSS), [R
Markdown](https://rstudio.github.io/thematic/articles/auto.html#rmd)
(via **bslib**), and
[RStudio](https://rstudio.github.io/thematic/articles/auto.html#rstudio)
(via [RStudio
themes](https://support.rstudio.com/hc/en-us/articles/115011846747-Using-RStudio-Themes)).
For a quick example, here’s a `shiny::tabsetPanel()` with custom CSS
styling, but default R styling:

``` r
library(shiny)
library(ggplot2)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(
    bg = "#002B36", fg = "#EEE8D5", primary = "#2AA198", 
    base_font = font_google("Pacifico")
  ),
  tabsetPanel(
    type = "pills",
    tabPanel("ggplot", plotOutput("ggplot")),
    tabPanel("lattice", plotOutput("lattice")),
    tabPanel("base", plotOutput("base"))
  )
)

server <- function(input, output) {
  output$ggplot <- renderPlot({
    ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars), color = factor(cyl))) +
      geom_point() +
      ggrepel::geom_text_repel()
  })
  output$lattice <- renderPlot({
    lattice::show.settings()
  })
  output$base <- renderPlot({
    image(volcano, col = thematic_get_option("sequential"))
  })
}

thematic_shiny()
shinyApp(ui, server)
```

<img src="man/figures/auto-before.png" width="80%" style="display: block; margin: auto;" />

To add automatic coloring and fonts (i.e., the full auto theming
experience) to the R plots, simply call `thematic_on(font = "auto")` and
re-run the application. Since the plots are generated via Shiny, they
assume new defaults which are informed by the CSS styling on their HTML
container (that is, notice how the R plots now reflect the styling of
the `shiny::tabsetPanel()`). Moreover, as long as the relevant font is a
[Google Font](https://fonts.google.com) (in this case,
[Pacifico](https://fonts.google.com/specimen/Pacifico)), **thematic**
automatically downloads, caches, and registers font(s) with R.

``` r
thematic_shiny(font = "auto")
shinyApp(ui, server)
```

<img src="man/figures/auto-after.png" width="80%" style="display: block; margin: auto;" />

Instead of relying on **thematic** to automatically detect colors and
fonts in the plot’s container, you can also specify them directly in
`thematic_on()`.

``` r
thematic_on(bg = "#222222", fg = "white", accent = "#0CE3AC", font = "Oxanium")

library(ggplot2)
ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars), color = factor(cyl))) +
  geom_point() +
  ggrepel::geom_text_repel()
```

<img src="man/figures/README-ggrepel-1.png" width="80%" style="display: block; margin: auto;" />

In addition to `thematic_on()`, which applies the provided theme to all
plots (up until `thematic_off()` is called), there are a few variation
of `thematic_on()` which temporarily apply the given theme:

  - `thematic_shiny()`: apply theme up until the next [Shiny](#shiny)
    app exits. Use this over `thematic_on()` in Shiny apps.
  - `thematic_rmd()`: apply theme up until the next [R Markdown](#rmd)
    document exits. Use this over `thematic_on()` in R Markdown
    documents.
  - `thematic_with_theme()`: apply theme up until the provided plot
    `expr` is evaluated. Use this to apply different themes to different
    plots within a Shiny app.

## Learn more

  - See the [auto theming
    article](https://rstudio.github.io/thematic/articles/auto.html) to
    gain an understanding of how auto theming make styling R plots
    easier in Shiny, R Markdown, and RStudio.
  - See the [custom themes
    article](https://rstudio.github.io/thematic/articles/custom.html)
    for more on **thematic**’s theming options as well as how they
    interact with **ggplot2**, **lattice**, and **base**.
  - See the [fonts
    article](https://rstudio.github.io/thematic/articles/fonts.html) for
    more on using Google Fonts with **thematic**.
  - See the [scoping
    article](https://rstudio.github.io/thematic/articles/scope.html) for
    more about restoring state after using **thematic**.

## Run some examples

Below is a link to an **RStudio Cloud** instance with some ready to run
**thematic** examples:

<div>

<a href="https://rstudio.cloud/project/1208127" target="_blank">
<img src="man/figures/thematic-test-drive.svg" alt="RStudio Cloud Example" height="80px" style="display: block; margin: 0 auto;">
</a>

</div>

## Code of Conduct

**thematic** is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By
[contributing](https://github.com/rstudio/thematic/blob/master/CONTRIBUTING.md)
to this project, you agree to abide by its terms.
