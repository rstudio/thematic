# thematic 0.1.2

* Closed #89: `{thematic}` now works as expected with the `{gganimate}` package. (#96)
* Closed #93: Better support for `{ggplot2}` extensions that set geom/scale colour defaults to 0.
* Closed #90: Enabling thematic no longer fails when used with package(s) that register `{ggplot2}` methods via `registerS3method()` within `.onLoad()` (`{zoo}` is one such package that does this). (#90)
* Closed #86: `thematic_rmd()` now works as expected in `shiny: runtime` Rmd documents. (#87)
* Closed #82: Fixed a bug with `ggplot2::element_blank()` in plot-specific user code not being respected by `{thematic}`. (#83) 

# thematic 0.1.1

* Initial release of the package, see https://rstudio.github.io/thematic/
