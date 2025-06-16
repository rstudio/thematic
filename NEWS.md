# thematic 0.1.7

* Updates to accomodate new ggplot2 v4.0.0 release. (#156)

# thematic 0.1.6

* Add basic positron IDE support. (#152)
* Update for now deprecated `systemfonts::match_font()`. (#153)

# thematic 0.1.5

* Accommodate for breaking changes in ggplot2 v3.5.0. (#148)
* `{thematic}` now works with recent versions of the `{vdiffr}` package. (#149)

# thematic 0.1.4

* The `{httpgd}` graphics device (and thus, Github codespaces) is now supported. (#143)

# thematic 0.1.3

* Closed #129: Fixed a bug with not being able override thematic inside geom_sf(). (#129)
* Closed #120: A warning about a `length-one vector` is no longer thrown on R 4.2. (#121)
* Closed #111: `{thematic}` no longer throws an error when used with `ggiraph::dsvg()`. (#112)

# thematic 0.1.2.1

Patch release for vdiffr 1.0

# thematic 0.1.2

* Closed #89: `{thematic}` now works as expected with the `{gganimate}` package. (#96)
* Closed #93: Better support for `{ggplot2}` extensions that set geom/scale colour defaults to 0.
* Closed #90: Enabling thematic no longer fails when used with package(s) that register `{ggplot2}` methods via `registerS3method()` within `.onLoad()` (`{zoo}` is one such package that does this). (#90)
* Closed #86: `thematic_rmd()` now works as expected in `shiny: runtime` Rmd documents. (#87)
* Closed #82: Fixed a bug with `ggplot2::element_blank()` in plot-specific user code not being respected by `{thematic}`. (#83) 

# thematic 0.1.1

* Initial release of the package, see https://rstudio.github.io/thematic/
