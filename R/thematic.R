#' Automatic theming of static plots
#'
#' Enable (or disable) automatic theming of ggplot2, lattice, and base graphics.
#' While enabled, thematic registers [plot.new()]/[grid.newpage()] hooks
#' that set relevant options (i.e., [graphics::par()], [grid::gpar()],
#' [lattice::trellis.par.set()], [ggplot2::theme_set()], etc) based on
#' the current context.
#'
#' @section Resolving 'auto' values:
#'
#' The `bg`, `fg`, `accent`, and `font` arguments all support a value of `'auto'`.
#' In this case, thematic does it's best to use information based on the current
#' plotting context to inform relevant graphical parameters. The order of priority
#' for resolving `'auto'` is as follows:
#'
#' 1. If running inside `shiny::renderPlot()`, use `shiny::getCurrentOutputInfo()`.
#' 2. If set, use [auto_preferences_set()].
#' 3. If running inside `rmarkdown::html_document()` with `theme = NULL`,
#'   use `bootstraplib::bs_theme_get_variables()`.
#' 4. If running inside RStudio, use `rstudioapi::getThemeInfo()`.
#'
#' @details
#'
#' Colors may be anything understood by [col2rgb()] or `htmltools::parseCssColors()`
#' (i.e., may be any valid R or CSS color string).
#'
#' @param bg a background color.
#' @param fg a foreground color.
#' @param accent a color for making certain graphical markers 'stand out'
#' (e.g., the fitted line color for [ggplot2::geom_smooth()]).
#' Can be 2 colors for lattice (stroke vs fill accent).
#' @param font a `font_spec()` object. If missing, font defaults are not altered.
#' @param sequential a color palette for graphical markers that encode
#' numeric values. Can be a vector of color codes or a
#' [sequential_gradient()] object.
#' @param qualitative a color palette for graphical markers that encode
#' qualitative values (won't be used in ggplot2 when the number of data
#' levels exceeds the max allowed colors). Defaults to the Okabe-Ito colorscale.
#'
#' @return Returns any information about the previously set theme (if any), invisibly.
#'
#' @rdname thematic
#' @seealso [font_spec()], [thematic_with_device()], [thematic_get()]
#' @export
#' @examples
#' # simple dark mode
#' thematic_begin("black", "white")
#' plot(1:10)
#' plot(1:10, col = 1:10)
#' lattice::show.settings()
#'
#' # use any color code
#' thematic_begin("#444444", "#e4e4e4")
#' plot(1:10)
#' plot(1:10, col = 1:10)
#' lattice::show.settings()
#'
#' # restores _original_ state
#' thematic_end()
#' plot(1:10)
#' lattice::show.settings()
#'
#' thematic_begin("darkblue", "skyblue", "orange")
#' image(volcano)
#' image(volcano, col = thematic_get_option("sequential"))
#' lattice::show.settings()
#' thematic_end()
#'
thematic_begin <- function(bg = "auto", fg = "auto", accent = "auto",
                           font = NA, sequential = sequential_gradient(),
                           qualitative = okabe_ito()) {
  old_theme <- .globals$theme
  .globals$theme <- list(
    bg = bg, fg = fg, accent = accent,
    qualitative = qualitative, sequential = sequential,
    font = as_font_spec(font)
  )

  # Set knitr dev.args = list(bg = bg) now (instead of later)
  # so at least the _next_ chunk has the right bg color.
  knitr_dev_args_set()

  # Register thematic hooks (these hooks modify global state when a new page is drawn)
  set_hooks()
  # Register showtext hooks (for custom font rendering in non-ragg devices)
  if (is_installed("showtext")) showtext::showtext_auto()

  # Override ggplot build method mainly because we currently need access to
  # the plot object in order to set Geom/Scale defaults
  ggplot_build_set()
  lattice_print_set()

  invisible(old_theme)
}

#' @rdname thematic
#' @export
thematic_end <- function() {
  remove_hooks()
  if (is_installed("showtext")) showtext::showtext_auto(FALSE)

  # Removing the plot.new hooks is not enough to restore global state
  base_params_restore()
  base_palette_restore()
  knitr_dev_args_restore()
  ggplot_theme_restore()
  ggplot_build_restore()
  lattice_print_restore()

  if (!is.null(.globals$theme)) rm("theme", envir = .globals)

  invisible()
}

#' Query the current thematic theme
#'
#' [thematic_get()] returns information about the entire theme,
#' whereas [thematic_get_option()] returns information about a specific
#' option.
#'
#' @export
#' @examples
#' thematic_get()
#' thematic_begin("darkblue", "skyblue")
#' thematic_get_option("bg")
#'
#' if (interactive()) {
#'   scales::show_col(thematic_get_mixture(seq(0, 1, by = 0.1)))
#' }
#'
thematic_get <- function() {
  .globals$theme
}

#' @rdname thematic_get
#' @param name a theme element name (e.g., `fg`, `bg`, etc.)
#' @param default a default value to return in the event no thematic theme is active.
#' @export
thematic_get_option <- function(name = "", default = NULL) {
  if (length(name) != 1) {
    stop("`name` must be length 1", call. = FALSE)
  }
  theme_names <- names(.globals$theme)
  if (length(theme_names) && !name %in% theme_names) {
    stop(
      sprintf(
        "`name` must be one of the following: '%s'",
        paste(theme_names, collapse = "', '")
      ),
      call. = FALSE
    )
  }
  .globals$theme[[name]] %||% default
}

#' @rdname thematic_get
#' @param amounts value(s) between 0 and 1 specifying how much to mix `fg` (0) and `bg` (1).
#' @export
thematic_get_mixture <- function(amounts = 0.5) {
  if (any(amounts < 0 | amounts > 1)) {
    stop("`amounts` must be between 0 and 1", call. = FALSE)
  }
  fg <- thematic_get_option("fg")
  bg <- thematic_get_option("bg")
  if (!length(fg) || !length(bg)) {
    return(NULL)
  }
  scales::colour_ramp(c(fg, bg))(amounts)
}

#' Font specification
#'
#' Specify a collection of font families. The first font family supported
#' by the relevant device (i.e., the device that is open, or will be opened, at
#' plotting time) is used by thematic. If a given font family is not supported
#' by the default, but is a [Google Font](https://fonts.google.com/) and
#' `install = TRUE`, the font will be downloaded, cached, and registered
#' for use the **showtext** and **ragg** packages.
#'
#' @param families a character vector of font families.
#' @param scale numerical constant applied to font sizes.
#' @param install whether to download and register font `families`
#' available via [Google Fonts](https://fonts.google.com/) (but unavailable to R).
#' After a successful download, fonts are cached (in a directory which
#' can be managed via [font_cache_set()]), and registered for use with
#' the **showtext** and **ragg** packages. If installation fails with
#' a valid internet connection, you may need to fetch the latest Google
#' Font information prior to installation (i.e., set `update = TRUE`).
#' @param update if `TRUE`, the latest Google Fonts are fetched and
#' any out-dated font cache is updated. Fetching the latest fonts requires
#' a Google Font API key (one is bundled with the package, but you can
#' set your own via an environment variable, `GFONT_KEY`).
#' @param quiet whether to suppress download messages.
#'
#' @return the input arguments as a list.
#' @seealso [thematic_with_device()], [thematic_begin()], [font_cache_set()]
#'
#' @export
font_spec <- function(families = "", scale = 1, install = is_installed("ragg") || is_installed("showtext"),
                      update = FALSE, quiet = TRUE) {

  if (update) {
    update_gfonts()
    update_gfonts_cache()
  }

  structure(
    list(families = families, scale = scale, install = install, quiet = quiet),
    class = "font_spec"
  )
}

is_font_spec <- function(x) {
  inherits(x, "font_spec")
}

as_font_spec <- function(font) {
  if (is_font_spec(font)) return(font)
  if (isTRUE(is.na(font))) return(font_spec())
  if (is.character(font)) return(font_spec(font))

  stop("`font` must be either `NA`, a `font_spec()` object, or a character vector", call. = FALSE)
}

is_default_spec <- function(font) {
  identical(as_font_spec(font), font_spec())
}


#' Okabe Ito colorscale
#'
#' @param n number of colors.
#'
#' @return a vector of color codes.
#' @seealso [thematic_begin()]
#' @references \url{https://jfly.uni-koeln.de/color/}
#' @export
okabe_ito <- function(n = NULL) {
  okabeIto <- c("#E69F00", "#009E73", "#0072B2", "#CC79A7", "#999999", "#D55E00", "#F0E442", "#56B4E9")
  if (is.null(n)) okabeIto else okabeIto[seq_len(n)]
}

#' Construct a sequential colorscale from fg, accent, and bg
#'
#' Controls the default weighting and direction of the color gradient
#' derived from the `fg`, `bg`, and `accent` color (defined in `thematic_begin()`).
#'
#' @param fg_weight a number (between 0 and 1) defining much of the `fg`
#' color should be mixed into the colourscale.
#' @param bg_weight a number (between 0 and 1) defining much of the `bg`
#' color should be mixed into the colourscale.
#' @param fg_low if `TRUE` (the default), the `fg` color is used for the
#' low end of the colorscale (rather than the high end).
#' @param n number of color codes.
#' @return a list of options for passing to the `sequential` argument of [thematic_begin()].
#' @export
#' @examples
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   # Gradient from fg to accent
#'   fg <- sequential_gradient(1, 0)
#'   thematic_begin("black", "white", "salmon", sequential = fg)
#'   ggplot2::qplot(1:10, 1:10, color = 1:10)
#'
#'   # Gradient from accent -> bg
#'   bg <- sequential_gradient(0, 1)
#'   thematic_begin("black", "white", "salmon", sequential = bg)
#'   ggplot2::qplot(1:10, 1:10, color = 1:10)
#'
#'   # Gradient from mix(accent, fg, 0.5) -> mix(accent, bg, 0.5)
#'   mix <- sequential_gradient(0.5, 0.5)
#'   thematic_begin("black", "white", "salmon", sequential = mix)
#'   ggplot2::qplot(1:10, 1:10, color = 1:10)
#'
#'   # Use fg (instead of bg) for high end of scale
#'   mix_flip <- sequential_gradient(0.5, 0.5, fg_low = FALSE)
#'   thematic_begin("black", "white", "salmon", sequential = mix_flip)
#'   ggplot2::qplot(1:10, 1:10, color = 1:10)
#' }
#'
sequential_gradient <- function(fg_weight = 0.75, bg_weight = 0.5, fg_low = TRUE, n = 30) {
  # TODO: return a list with special class that gets resolved at theme_create() time
  if (any(fg_weight > 1 | fg_weight < 0)) {
    stop("`fg_weight` must be between 0 and 1.", call. = FALSE)
  }
  if (any(bg_weight > 1 | bg_weight < 0)) {
    stop("`bg_weight` must be between 0 and 1.", call. = FALSE)
  }
  if (n < 3) {
    stop("`n` must be 3 or more.", call. = FALSE)
  }
  if (!is.logical(fg_low)) {
    stop("`fg_low` must be `TRUE` or `FALSE`", call. = FALSE)
  }
  structure(
    list(fg_weight = fg_weight, bg_weight = bg_weight, fg_low = fg_low, n = n),
    class = "thematic_sequential_options"
  )
}


# Main idea: Interpolate between [fg+accent -> accent -> bg+accent]
# For the endpoints the amount of blending of fg/bg and accent
# depends on their perceptual distance
resolve_sequential_gradient <- function(fg, accent, bg, options = sequential_gradient()) {
  accent <- accent[1]
  if (anyNA(c(fg, accent, bg))) return(NA)

  fg_dist <- farver::compare_colour(
    farver::decode_colour(fg), farver::decode_colour(accent),
    from_space = "rgb", method = "cie2000"
  )
  bg_dist <- farver::compare_colour(
    farver::decode_colour(bg), farver::decode_colour(accent),
    from_space = "rgb", method = "cie2000"
  )
  total_dist <- bg_dist + fg_dist

  rng <- if (options$fg_low) {
    c(
      -options$fg_weight * as.numeric(fg_dist / total_dist),
      options$bg_weight * as.numeric(bg_dist / total_dist)
    )
  } else {
    c(
      -options$bg_weight * as.numeric(bg_dist / total_dist),
      options$fg_weight * as.numeric(fg_dist / total_dist)
    )
  }
  grid <- scales::rescale(
    seq(0, 1, length.out = options$n),
    to = pmax(pmin(rng + 0.5, 1), 0)
  )
  cols <- if (options$fg_low) c(fg, accent, bg) else c(bg, accent, fg)
  scales::colour_ramp(cols, alpha = TRUE)(grid)
}
