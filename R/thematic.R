#' Theme static plots based on a few colors
#'
#' Theme ggplot2, lattice, and base graphics based on just a few colors
#' supplied to [thematic_begin()]. [thematic_begin()] works by modifying global
#' state (e.g., sets relevant options in [graphics::par()], [grid::gpar()],
#' [lattice::trellis.par.set()], [ggplot2::theme_set()], etc). To restore
#' global state to the state before [thematic_begin()] was called,
#' use [thematic_end()]. To inspect the current thematic state, use
#' [thematic_current()] (this can be particularly useful for routing the
#' sequential color palette to base/lattice graphics).
#'
#' Colors may be anything understood by [col2rgb()] or `htmltools::parseCssColors()`
#' (i.e., may be any valid R or CSS color string).
#'
#' @param bg a background color.
#' @param fg a foreground color.
#' @param accent a color for making certain graphical markers 'stand out'
#' (e.g., the fitted line color for [ggplot2::geom_smooth()]).
#' Can be 2 colors for lattice (stroke vs fill accent).
#' @param font a `font_spec()` object.
#' @param sequential color palette for numeric variables.
#' Defaults to a gradient based on `accent` color.
#' @param qualitative color palette for discrete variables.
#' Defaults to the Okabe-Ito colorscale (won't be used in ggplot2 when
#' the number of data levels exceeds the max allowed colors).
#'
#' @return Returns any information about the previously set theme (if any), invisibly.
#'
#' @rdname thematic
#' @seealso [font_spec()], [thematic_with_device()]
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
#' image(volcano, col = thematic_current("sequential"))
#' lattice::show.settings()
#' thematic_end()
#'
thematic_begin <- function(bg = NULL, fg = NULL, accent = NA,
                           font = font_spec(),
                           sequential = sequential_gradient(fg = fg, accent = accent, bg = bg),
                           qualitative = okabe_ito()) {
  old_theme <- .globals$theme
  .globals$theme <- theme_create(
    bg = bg, fg = fg, accent = accent,
    qualitative = qualitative, sequential = sequential,
    font = font
  )
  # Register thematic hooks (these hooks modify global state when a new page is drawn)
  set_hooks()
  # Register showtext hooks (for custom font rendering in non-ragg devices)
  if (is_installed("showtext")) showtext::showtext_auto()

  # Override ggplot print method mainly because we currently need access to
  # the plot object in order to set Geom/Scale defaults
  ggplot_print_set()

  knitr_dev_args_set()

  invisible(old_theme)
}

#' @rdname thematic
#' @export
thematic_end <- function() {
  if (!is.null(.globals$theme)) rm("theme", envir = .globals)
  remove_hooks()
  if (is_installed("showtext")) showtext::showtext_auto(FALSE)

  # Removing the plot.new hooks is not enough to restore global state
  base_params_restore()
  base_palette_restore()
  knitr_dev_args_restore()
  ggplot_theme_restore()
  ggplot_print_restore()
  lattice_print_restore()

  invisible()
}

#' @rdname thematic
#' @param which which theme element (i.e., which argument of [thematic_begin()]?).
#' Defaults to all theme elements.
#' @export
thematic_current <- function(which = "all") {
  if (identical("all", which)) .globals$theme else .globals$theme[[which]]
}


# TODO: Use a class?
theme_create <- function(bg, fg, accent, qualitative, sequential, font) {
  colors <- list(
    bg = bg, fg = fg, accent = accent,
    qualitative = qualitative, sequential = sequential
  )
  theme <- lapply(colors, function(x) {
    if (identical(x, NA)) return(x)
    vapply(x, parse_any_color, character(1), USE.NAMES = FALSE)
  })
  if (!inherits(font, "font_spec")) {
    stop("The `font` argument must be a `font_spec()` object", call. = FALSE)
  }
  theme$font <- font
  theme
}


#' Font specification
#'
#' Specify a collection of font families. The first font family supported
#' by the relevant device (i.e., the device that is open, or will be opened, at
#' plotting time) is used by thematic. If a given font family is not supported
#' by the default, but is a [Google Font](https://fonts.google.com/) and
#' `auto_install = TRUE`, the font will be downloaded, cached, and registered
#' for use the **showtext** and **ragg** packages.
#'
#' @param families a character vector of font families.
#' @param scale numerical constant applied to font sizes.
#' @param auto_install whether or not to attempt automatic download and registration
#' of fonts not found on the system. Currently any font on Google Fonts is supported.
#'
#' @return a list of information about the font specification.
#' @seealso [thematic_with_device()], [thematic_begin()], [font_cache_set()]
#'
#' @export
font_spec <- function(families = "", scale = 1, auto_install = is_installed("ragg") || is_installed("showtext")) {
  structure(
    list(families = families, scale = scale, auto_install = auto_install),
    class = "font_spec"
  )
}

is_default_family <- function(x) {
  identical(x, "")
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
#' @inheritParams thematic_begin
#' @param alpha a number between 0 and 1. Controls how close the endpoints
#' should be to the `fg` and `bg`.
#' @param n the number of color codes to return
#' @return a vector of color codes.
#' @seealso [thematic_begin()]
#' @export
sequential_gradient <- function(alpha = 0.5, n = 30, fg, accent, bg) {
  if (anyNA(c(fg, accent, bg))) return(NA)
  if (alpha > 1 || alpha < 0) {
    stop("alpha must be between 0 and 1", call. = FALSE)
  }

  # Main idea: Interpolate between [fg+accent -> accent -> bg+accent]
  # For the endpoints the amount of blending of fg/bg and accent
  # depends on how similar thwt
  fg_dist <- farver::compare_colour(
    farver::decode_colour(fg), farver::decode_colour(accent),
    from_space = "rgb", method = "cie2000"
  )
  bg_dist <- farver::compare_colour(
    farver::decode_colour(bg), farver::decode_colour(accent),
    from_space = "rgb", method = "cie2000"
  )
  total_dist <- bg_dist + fg_dist
  vals <- scales::rescale(
    seq(0, 1, length.out = n),
    to = 0.5 + c(
      -alpha * as.numeric(fg_dist / total_dist),
      alpha * as.numeric(bg_dist / total_dist)
    )
  )
  scales::colour_ramp(c(fg, accent, bg), alpha = TRUE)(vals)
}
