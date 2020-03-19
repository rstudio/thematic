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
#' @param bg a background color.
#' @param fg a foreground color.
#' @param accent a color for making certain graphical markers 'stand out'
#' (e.g., the fitted line color for [ggplot2::geom_smooth()]).
#' Can be 2 colors for lattice (stroke vs fill accent).
#' @param qualitative color palette for discrete variables.
#' Defaults to the Okabe-Ito colorscale (won't be used in ggplot2 when
#' the number of data levels exceeds the max allowed colors).
#' @param sequential color palette for numeric variables.
#' Defaults to a gradient based on `accent` color.
#'
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
                           qualitative = okabe_ito(),
                           sequential = sequential_gradient(fg, accent, bg),
                           font = font_spec()) {
  # Destroy the previous changes before starting new changes
  theme <- theme_create(
    bg = bg, fg = fg, accent = accent,
    qualitative = qualitative, sequential = sequential,
    font = font
  )
  thematic_end()
  .globals$theme <- theme
  # These functions modify global state in a way that is
  # (hopefully) independent of the graphics device state
  base_palette_set(theme)
  ggplot_theme_set(theme)
  ggplot_print_set(theme)
  ggplot_grob_set(theme)
  lattice_print_set(theme)
  # Set base graphical parameters now, and also the next time
  # plot.new()/grid.newpage() is called, which is necessary because
  # each device has it's own set of graphical parameters,
  # and opening a new device modifies the global state of said parameters
  base_params_set(theme)
  set_hooks()
  # For getting auto-installed fonts to render in non-ragg devices
  if (rlang::is_installed("showtext")) showtext::showtext_auto()
  invisible(theme)
}


#' @rdname thematic_begin
#' @param family font family.
#' @param scale numerical constant applied to font sizes.
#' @param auto_install whether or not to attempt automatic download and registration
#' of fonts not found on the system. Currently any font on Google Fonts is supported.
#' @export
font_spec <- function(family = "", scale = 1, auto_install = rlang::is_installed("ragg") || rlang::is_installed("showtext")) {
  list(family = family, scale = scale, auto_install = auto_install)
}

is_default_family <- function(family) {
  identical(family, "")
}

#' @rdname thematic_begin
#' @export
thematic_end <- function() {
  base_palette_restore()
  ggplot_theme_restore()
  ggplot_print_restore()
  ggplot_grob_restore()
  lattice_print_restore()
  base_params_restore()
  restore_hooks()
  if (rlang::is_installed("showtext")) showtext::showtext_auto(FALSE)
  if (!is.null(.globals$theme)) rm("theme", envir = .globals)
  invisible()
}

#' @rdname thematic_begin
#' @param which which theme element (i.e., which argument of [thematic_begin()]?).
#' Defaults to all theme elements.
#' @export
thematic_current <- function(which = "all") {
  if (identical("all", which)) .globals$theme else .globals$theme[[which]]
}


#' @rdname thematic_begin
#' @param expr an expression that produces a plot.
#' @param device a graphics device to use for capturing the plot
#' @param width
#' @param height
#' @param ... arguments to the graphics `device`.
#' @inheritParams thematic_begin
#' @export
#' @examples
#'
#' library(thematic)
#' font <- font_spec(family = "Rock Salt", scale = 1.25)
#' thematic_begin("black", "white", font = font)
#' file <- thematic_with_device(plot(1:10), res = 144)
#' if (interactive()) browseURL(file)
thematic_with_device <- function(expr, device = safe_device(),
                                 filename = tempfile(fileext = ".png"), ...) {
  # N.B. implementation is quite similar to htmltools::capturePlot
  if (!is.function(device)) {
    stop(call. = FALSE, "The `device` argument should be a function, e.g. `ragg::agg_png`")
  }

  isTempFile <- missing(filename)

  # collect user and device arguments
  args <- rlang::list2(filename = filename, ...)
  device_args <- names(formals(device))

  # do our best to find the background color arg
  bg_arg <- grep("^background$|^bg$", device_args, value = TRUE)
  if (!length(bg_arg)) {
    stop(
      "Wasn't able to detect the background color argument for the given device, ",
      "so thematic won't automatically set it for you, but you can also set it yourself ",
      "by doing `thematic_with_device(expr, bg_color_arg = thematic_current('bg'))`",
      call. = FALSE
    )
  }

  if (!is.null(args[[bg_arg]])) {
    warning(
      "Did you intend to specify the background color? ",
      "Thematic will set the background for you based on the current theme.",
      call. = FALSE
    )
  } else {
    args[[bg_arg]] <- thematic_current("bg") %||% "white"
  }

  # Handle the case where device wants `file` instead of `filename`
  # (e.g., svglite::svglite)
  if (!"filename" %in% device_args && "file" %in% device_args) {
    args$file <- args$filename
    args$filename <- NULL
  }

  # Device management
  do.call(device, args)
  dev <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(dev), add = TRUE)

  # make svglite happy (it doesn't support multiple pages and
  # it's convenient to support it for our own testing purposes
  if (!identical(device, getFromNamespace("svglite", "svglite"))) {
    op <- graphics::par(mar = rep(0, 4))
    grDevices::devAskNewPage(FALSE)
    tryCatch(graphics::plot.new(), finally = graphics::par(op))
  }

  # Evaluate the expression
  expr <- rlang::enquo(expr)
  tryCatch({
    result <- withVisible(rlang::eval_tidy(expr))
    if (result$visible) {
      capture.output(print(result$value))
    }
    filename
  }, error = function(e) {
    try({
      # i.e., if we _know_ this is a tempfile remove it before throwing
      if (isTempFile && file.exists(filename))
        unlink(filename)
    })
    stop(e)
  })
}

safe_device <- function(type = c("png", "tiff", "ppm")) {
  type <- match.arg(type)

  if (rlang::is_installed("ragg")) {
    dev <- switch(
      type,
      png = ragg::agg_png,
      tiff = ragg::agg_tiff,
      ppm = ragg::agg_ppm
    )
    return(dev)
  }

  if (!rlang::is_installed("showtext")) {
    message("Auto-installation of custom fonts requires either the showtext or ragg package.")
  }

  switch(
    type,
    png = grDevices::png,
    tiff = grDevices::tiff,
    stop("'", type, "' graphics device not available.", call. = )
  )
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
  theme$font <- font

  # Default font family doesn't require any special handling
  if (!is_default_family(font$family)) {
    theme$font$family <- resolve_font_families(font$family, font$auto_install)
  }

  theme
}


#' Okabe Ito colorscale
#'
#' @param n number of colors.
#'
#' @export
#' @references \url{https://jfly.uni-koeln.de/color/}
okabe_ito <- function(n = NULL) {
  okabeIto <- c("#E69F00", "#009E73", "#0072B2", "#CC79A7", "#999999", "#D55E00", "#F0E442", "#56B4E9")
  if (is.null(n)) okabeIto else okabeIto[seq_len(n)]
}

# TODO: export?
sequential_gradient <- function(fg, accent, bg, n = 30) {
  if (anyNA(c(fg, accent, bg))) return(NA)

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
      -0.5 * as.numeric(fg_dist / total_dist),
      0.4 * as.numeric(bg_dist / total_dist)
    )
  )
  scales::colour_ramp(c(fg, accent, bg), alpha = TRUE)(vals)
}
