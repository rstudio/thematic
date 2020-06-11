#' Enable (or disable) simplified theming of R graphics.
#'
#' A unified interface for theming **ggplot2**, **base**, and **lattice** graphics
#' based on a handful of styling options. In some cases (most notably in a **shiny** runtime),
#' these options can automatically resolve to relevant CSS styles (see the "Auto theming"
#' section below).
#'
#' @section Auto theming:
#'
#' The `bg`, `fg`, `accent`, and `font` arguments all support a value of `'auto'`,
#' which are all resolved, at plot time, based on the execution environment. In a
#' **shiny** runtime, resolution of auto values should always work as expect; but
#' in other contexts, auto values may lead to wrong or surprising results. In that
#' case, auto resolution logic can be customized (see [auto_config_set()] for more details).
#'
#' @section Global vs. local theming:
#'
#' `thematic_on()` enables thematic in a global fashion (that is, it impacts all
#' future plots, up until `thematic_off()` is called). To use thematic in local fashion,
#' first create a theme with [thematic_theme()], then provide it to [thematic_with_theme()]
#' (or similar). To use thematic in a global fashion up until a **shiny**
#' app exits, use `thematic_shiny()` (which cleans up after itself once the next shiny
#' app that exits using [shiny::onStop()]). To use thematic in a global fashion up until
#' a **rmarkdown** document finishes rendering, use `thematic_rmd()`.
#'
#' @section Color values:
#'
#' Colors (e.g., `bg`, `fg`, `accent`) may be any value understood by [col2rgb()]
#' or `htmltools::parseCssColors()` (i.e., may be any valid R or CSS color string).
#'
#' @param bg a background color.
#' @param fg a foreground color.
#' @param accent a color for making certain graphical markers 'stand out'
#' (e.g., the fitted line color for [ggplot2::geom_smooth()]).
#' Can be 2 colors for lattice (stroke vs fill accent).
#' @param font a [`font_spec()`] object. If missing, font defaults are not altered.
#' @param sequential a color palette for graphical markers that encode
#' numeric values. Can be a vector of color codes or a
#' [sequential_gradient()] object.
#' @param qualitative a color palette for graphical markers that encode
#' qualitative values (won't be used in ggplot2 when the number of data
#' levels exceeds the max allowed colors). Defaults to [okabe_ito()].
#' @param inherit should non-specified values inherit from the previous theme?
#'
#' @return [thematic_theme()] returns a theme object as a list (which can be
#' activated with [thematic_with_theme()] or [thematic_set_theme()]).
#'
#' [thematic_on()], [thematic_off()], and [thematic_shiny()] all return
#' the previous global theme.
#'
#' @rdname thematic
#' @seealso [sequential_gradient()], [thematic_with_theme()], [thematic_save_plot()]
#' @export
#' @examples
#' # simple dark mode
#' thematic_on("black", "white")
#' plot(1:10)
#' plot(1:10, col = 1:10)
#' lattice::show.settings()
#'
#' # use any hex color string
#' thematic_on("#444444", "#e4e4e4")
#' plot(1:10)
#' plot(1:10, col = 1:10)
#' lattice::show.settings()
#'
#' # disables thematic (also restores global state)
#' thematic_off()
#' plot(1:10)
#' lattice::show.settings()
#'
#' thematic_on("darkblue", "skyblue", "orange")
#' image(volcano)
#' image(volcano, col = thematic_get_option("sequential"))
#' lattice::show.settings()
#' thematic_off()
#'
thematic_on <- function(bg = "auto", fg = "auto", accent = "auto",
                        font = NA, sequential = sequential_gradient(),
                        qualitative = okabe_ito(), inherit = FALSE) {
  old_theme <- thematic_get_theme()
  .globals$theme <- thematic_theme(
    bg = bg, fg = fg, accent = accent,
    font = font, sequential = sequential,
    qualitative = qualitative, inherit = inherit
  )
  # Set knitr dev.args = list(bg = bg) now (instead of later)
  # so at least the _next_ chunk has the right bg color.
  knitr_dev_args_set()
  # Remember par("bg") now since bg can be modified by the opening of a
  # graphics device, and if that happens before plot time, it'd be too late
  # to base_restore_params()/base_set_params() at plot time
  .globals$base_params$bg <- par("bg")

  # Remove any existing hooks before registering them
  # (otherwise, repeated calls to set_hooks will keep adding them)
  remove_hooks()
  set_hooks()
  # Override ggplot build method mainly because we currently need access to
  # the plot object in order to set Geom/Scale defaults
  ggplot_build_set()
  lattice_print_set()

  invisible(old_theme)
}

#' @rdname thematic
#' @export
thematic_off <- function() {
  remove_hooks()

  # Removing the plot.new hooks is not enough to restore global state
  base_params_restore()
  base_palette_restore()
  knitr_dev_args_restore()
  ggplot_theme_restore()
  ggplot_build_restore()
  lattice_print_restore()

  theme <- .globals$theme
  if (!is.null(theme)) rm("theme", envir = .globals)

  invisible(theme)
}

#' @rdname thematic
#' @export
thematic_theme <- function(bg = "auto", fg = "auto", accent = "auto",
                           font = NA, sequential = sequential_gradient(),
                           qualitative = okabe_ito(), inherit = FALSE) {

  # This function is called at plot time (with bg/fg/accent)
  if (is.function(sequential)) {
    sequential <- structure("", sequential_func = sequential)
  }

  new <- structure(
    list(
      bg = tag_auto(bg),
      fg = tag_auto(fg),
      accent = tag_auto(accent),
      qualitative = qualitative,
      sequential = sequential,
      font = as_font_spec(font)
    ),
    class = "thematic_theme"
  )

  # Newly _specified_ theme elements override old elements
  old <- thematic_get_theme()
  if (isTRUE(inherit) && length(old)) {
    fmls <- formals(thematic_theme)
    if (identical(bg, fmls$bg)) new$bg <- NULL
    if (identical(fg, fmls$fg)) new$fg <- NULL
    if (identical(accent, fmls$accent)) new$accent <- NULL
    if (identical(sequential, fmls$sequential)) new$sequential <- NULL
    if (identical(qualitative, fmls$qualitative)) new$qualitative <- NULL
    new <- utils::modifyList(old, new)
    new <- structure(new, class = "thematic_theme")
  }

  new
}

is_thematic_theme <- function(x) {
  inherits(x, "thematic_theme")
}

#' @rdname thematic
#' @inheritParams thematic_on
#' @param session see [shiny::onStop()].
#' @export
thematic_shiny <- function(bg = "auto", fg = "auto", accent = "auto",
                           font = NA, sequential = sequential_gradient(),
                           qualitative = okabe_ito(), inherit = FALSE,
                           session = shiny::getDefaultReactiveDomain()) {
  old_theme <- thematic_on(
    bg = bg, fg = fg, accent = accent,
    font = font, sequential = sequential,
    qualitative = qualitative, inherit = inherit
  )
  shiny::onStop(function() thematic_set_theme(old_theme), session = session)
  invisible(old_theme)
}


#' @rdname thematic
#' @inheritParams thematic_on
#' @export
thematic_rmd <- function(bg = "auto", fg = "auto", accent = "auto",
                         font = NA, sequential = sequential_gradient(),
                         qualitative = okabe_ito(), inherit = FALSE) {
  old_theme <- thematic_on(
    bg = bg, fg = fg, accent = accent,
    font = font, sequential = sequential,
    qualitative = qualitative, inherit = inherit
  )
  document_hook <- knitr::knit_hooks$get("document")
  knitr::knit_hooks$set(document = function(x) {
    thematic_set_theme(old_theme)
    document_hook(x)
  })
  invisible(old_theme)
}


#' Tools for getting and restoring global state
#'
#' * [thematic_with_theme()]: similar to [thematic_on()], but for an single plot.
#' * [thematic_local_theme()]: similar to [thematic_with_theme()], but de-couples
#'   the theme from the plot expression.
#' * [thematic_set_theme()]: set a given `theme` object as the current theme.
#' * [thematic_get_theme()]: obtain the current `theme`.
#' * [thematic_get_option()]: obtain a particular `theme` option (and provide a `default`
#'   if if no `theme` is active).
#' * [thematic_get_mixture()]: obtain a mixture of the current `theme`'s `bg` and `fg`.
#'
#' @param theme a [thematic_theme()] object.
#' @param expr R code that produces a plot.
#' @param default a default value to return in the event no thematic theme is active.
#' @rdname theme-management
#' @export
#' @examples
#'
#' # Use thematic_with_theme() for a one-time use of thematic
#' thematic_with_theme(
#'   thematic_theme("darkblue", "skyblue", accent = "red"),
#'   plot(1:10, col = thematic_get_option("accent"), pch = 19)
#' )
#'
#' # Use thematic_set_theme() if doing something more complicated
#' # like programming on top thematic (without causing side effects)
#' my_plot <- function(expr, las = 3, ...) {
#'   old_theme <- thematic_on("black", "white")
#'   on.exit(thematic_set_theme(old_theme), add = TRUE)
#'   opts <- par(las = las)
#'   on.exit(par(opts), add = TRUE)
#'   # Imagine some more customization with ...
#'   force(expr)
#' }
#' my_plot(plot(1:10))
#'
#' thematic_off()
#' thematic_get_option("bg", "white")
#' thematic_on(bg = "red")
#' thematic_get_option("bg", "white")
#' thematic_off()
#'
#' thematic_with_theme(
#'   thematic_theme("darkblue", "skyblue"),
#'   scales::show_col(thematic_get_mixture(seq(0, 1, by = 0.1)))
#' )
#'
thematic_with_theme <- function(theme, expr) {
  old_theme <- thematic_set_theme(theme)
  on.exit(thematic_set_theme(old_theme), add = TRUE)
  # This is here so that users don't have to explictly call print()
  # on ggplot2/lattice objects (e.g., `renderPlot(thematic_with_theme(theme, qplot(1:10)))`)
  expr <- rlang::enquo(expr)
  result <- withVisible(rlang::eval_tidy(expr))
  if (result$visible) print(result$value)
  invisible(result$value)
}

#' @rdname theme-management
#' @param .local_envir The environment to use for scoping.
#' @export
thematic_local_theme <- function(theme, .local_envir = parent.frame()) {
  old_theme <- thematic_set_theme(theme)
  withr::defer(thematic_set_theme(old_theme), envir = .local_envir)
  invisible(old_theme)
}

#' @rdname theme-management
#' @param theme a `thematic_theme()` object (or a return value of [thematic_on]/[thematic_get_theme()])
#' or `NULL` (in which case `thematic_off()` is called).
#' @export
thematic_set_theme <- function(theme) {
  if (!length(theme)) {
    return(thematic_off())
  }
  if (!is_thematic_theme(theme)) {
    stop("`theme` must be a value returned by thematic_on() or thematic_get_theme().", call. = FALSE)
  }
  do.call(thematic_on, theme)
}

#' @rdname theme-management
#' @param resolve whether or not `'auto'` values should be resolved before returning
#' @export
thematic_get_theme <- function(resolve = TRUE) {
  if (resolve) {
    auto_resolve_theme(.globals$theme)
  } else {
    .globals$theme
  }
}

#' Get a thematic theme
#'
#' This function is deprecated. Use [thematic_get_theme()].
#'
#' @export
thematic_get <- function() {
  .Deprecated("thematic_get_theme")
  thematic_get_theme()
}

#' @rdname theme-management
#' @param name a theme element name (e.g., `fg`, `bg`, etc.)
#' @export
thematic_get_option <- function(name = "", default = NULL, resolve = TRUE) {
  if (length(name) != 1) {
    stop("`name` must be length 1", call. = FALSE)
  }
  theme <- thematic_get_theme(resolve = resolve)
  theme_names <- names(theme)
  if (length(theme) && length(setdiff(name, theme_names))) {
    stop(
      sprintf(
        "`name` must be one of the following: '%s'",
        paste(theme_names, collapse = "', '")
      ),
      call. = FALSE
    )
  }
  theme[[name]] %||% default
}

#' @rdname theme-management
#' @param amounts value(s) between 0 and 1 specifying how much to mix `bg` (0) and `fg` (1).
#' @export
thematic_get_mixture <- function(amounts = 0.5, default = NULL) {
  if (!length(thematic_get_theme())) {
    if (length(default)) {
      default <- rep_len(default, length(amounts))
    }
    return(default)
  }
  if (any(amounts < 0 | amounts > 1)) {
    stop("`amounts` must be between 0 and 1", call. = FALSE)
  }
  fg <- thematic_get_option("fg")
  bg <- thematic_get_option("bg")
  scales::colour_ramp(c(bg, fg))(amounts)
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
#' @seealso [thematic_save_plot()], [thematic_on()], [font_cache_set()]
#'
#' @export
font_spec <- function(families = "", scale = 1, install = is_installed("ragg") || is_installed("showtext"),
                      update = FALSE, quiet = TRUE) {

  if (update) {
    update_gfonts()
    update_gfonts_cache()
  }

  structure(
    list(
      families = tag_auto(families), scale = tag_auto(scale),
      install = install, quiet = quiet
    ),
    class = "font_spec"
  )
}

is_font_spec <- function(x) {
  inherits(x, "font_spec")
}

as_font_spec <- function(font) {
  if (is_font_spec(font)) return(font)
  if (isTRUE(is.na(font))) return(font_spec())
  if (identical(font, "auto")) return(font_spec(tag_auto(font), tag_auto(font)))
  if (is.character(font)) return(font_spec(font))

  stop("`font` must be either `NA`, a `font_spec()` object, or a character vector", call. = FALSE)
}


#' A color-blind safe qualitative colorscale (Okabe-Ito)
#'
#' This is the default `qualitative` colorscale in `thematic_on()`
#'
#' @param n number of colors.
#'
#' @return a vector of color codes.
#' @seealso [thematic_on()]
#' @references \url{https://jfly.uni-koeln.de/color/}
#' @export
okabe_ito <- function(n = NULL) {
  okabeIto <- c("#E69F00", "#009E73", "#0072B2", "#CC79A7", "#999999", "#D55E00", "#F0E442", "#56B4E9")
  if (is.null(n)) okabeIto else okabeIto[seq_len(n)]
}

#' Control parameters of the sequential colorscale
#'
#' Controls the default weighting and direction of the color gradient
#' derived from the `fg`, `bg`, and `accent` color (defined in `thematic_on()`).
#'
#' @param fg_weight a number (between 0 and 1) defining much of the `fg`
#' color should be mixed into the colorscale.
#' @param bg_weight a number (between 0 and 1) defining much of the `bg`
#' color should be mixed into the colorscale.
#' @param fg_low if `TRUE` (the default), the `fg` color is used for the
#' low end of the colorscale (rather than the high end).
#' @param n number of color codes.
#' @return a list of options for passing to the `sequential` argument of [thematic_on()].
#' @export
#' @examples
#'
#' # Gradient from fg to accent
#' fg <- sequential_gradient(1, 0)
#' thematic_on("black", "white", "salmon", sequential = fg)
#' ggplot2::qplot(1:10, 1:10, color = 1:10)
#'
#' # Gradient from accent -> bg
#' bg <- sequential_gradient(0, 1)
#' thematic_on("black", "white", "salmon", sequential = bg)
#' ggplot2::qplot(1:10, 1:10, color = 1:10)
#'
#' # Gradient from mix(accent, fg, 0.5) -> mix(accent, bg, 0.5)
#' mix <- sequential_gradient(0.5, 0.5)
#' thematic_on("black", "white", "salmon", sequential = mix)
#' ggplot2::qplot(1:10, 1:10, color = 1:10)
#'
#' # Use fg (instead of bg) for high end of scale
#' mix_flip <- sequential_gradient(0.5, 0.5, fg_low = FALSE)
#' thematic_on("black", "white", "salmon", sequential = mix_flip)
#' ggplot2::qplot(1:10, 1:10, color = 1:10)
#'
sequential_gradient <- function(fg_weight = 0.75, bg_weight = 0.5, fg_low = TRUE, n = 30) {
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

  # Main idea: Interpolate between [fg+accent -> accent -> bg+accent]
  # For the endpoints the amount of blending of fg/bg and accent
  # depends on their perceptual distance
  function(fg, accent, bg, ...) {
    accent <- accent[1]
    if (anyNA(c(fg, accent, bg))) return(NA)

    fg_dist <- farver::compare_colour(
      farver::decode_colour(fg), farver::decode_colour(accent),
      from_space = "rgb", method = "cmc"
    )
    bg_dist <- farver::compare_colour(
      farver::decode_colour(bg), farver::decode_colour(accent),
      from_space = "rgb", method = "cmc"
    )
    total_dist <- bg_dist + fg_dist

    rng <- if (fg_low) {
      c(
        -fg_weight * as.numeric(fg_dist / total_dist),
        bg_weight * as.numeric(bg_dist / total_dist)
      )
    } else {
      c(
        -bg_weight * as.numeric(bg_dist / total_dist),
        fg_weight * as.numeric(fg_dist / total_dist)
      )
    }
    grid <- scales::rescale(
      seq(0, 1, length.out = n),
      to = pmax(pmin(rng + 0.5, 1), 0)
    )
    cols <- if (fg_low) c(fg, accent, bg) else c(bg, accent, fg)
    scales::colour_ramp(cols, alpha = TRUE)(grid)
  }
}

