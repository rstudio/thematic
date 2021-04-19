# thematic 0.1.2.9000

## Potential breaking changes

`{thematic}`'s `{lattice}` support now works by modifying _global_ graphical parameters (via `trellis.par.set()`) instead of always supplying them locally to the plot's `par.settings` (and, as a result, `par.settings` can now be used to override `{thematic}`'s settings, thus closing #100). This approach is a more desirable since it allows for customization of `{thematic}`'s defaults, but beware it may 'break' some existing `{lattice}`+`{thematic}` code that unnecessarily specifies `par.settings` instead of allowing those settings to derive from `trellis.par.get()` (for examples, `show.settings()` should now be `show.settings(trellis.par.get())` in order to work sensible with `{thematic}`).

## Bug fixes

* Closed #100: `{lattice}`'s plot-specific `par.settings` argument now works as expected with `{thematic}` (it can be used to override any settings that `{thematic}` sets). (#101)

# thematic 0.1.2

* Closed #89: `{thematic}` now works as expected with the `{gganimate}` package. (#96)
* Closed #93: Better support for `{ggplot2}` extensions that set geom/scale colour defaults to 0.
* Closed #90: Enabling thematic no longer fails when used with package(s) that register `{ggplot2}` methods via `registerS3method()` within `.onLoad()` (`{zoo}` is one such package that does this). (#90)
* Closed #86: `thematic_rmd()` now works as expected in `shiny: runtime` Rmd documents. (#87)
* Closed #82: Fixed a bug with `ggplot2::element_blank()` in plot-specific user code not being respected by `{thematic}`. (#83) 

# thematic 0.1.1

* Initial release of the package, see https://rstudio.github.io/thematic/
