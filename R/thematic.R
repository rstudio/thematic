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
#' image(volcano, col = theme_current("sequential"))
#' lattice::show.settings()
#' thematic_end()
#'
thematic_begin <- function(bg = NULL, fg = NULL, accent = NA,
                        qualitative = okabe_ito(),
                        sequential = sequential_gradient(fg, accent, bg)) {
  # Destroy the previous changes before starting new changes
  thematic_end()
  theme <- theme_create(
    bg = bg, fg = fg, accent = accent,
    qualitative = qualitative, sequential = sequential
  )
  .globals$theme <- theme
  base_params_set(theme)
  base_palette_set(theme)
  grid_params_set(theme)
  lattice_params_set(theme)
  ggplot_theme_set(theme)
  ggplot_print_set(theme)
  invisible(theme)
}



#' @rdname thematic_begin
#' @export
thematic_end <- function() {
  if (!is.null(.globals$theme)) rm("theme", envir = .globals)
  base_params_restore()
  base_palette_restore()
  grid_params_restore()
  lattice_params_restore()
  ggplot_theme_restore()
  ggplot_print_restore()
  invisible()
}

#' @rdname thematic_begin
#' @param which which theme element (i.e., which argument of [thematic_begin()]?).
#' Defaults to all theme elements.
#' @export
thematic_current <- function(which = "all") {
  if (identical("all", which)) .globals$theme else .globals$theme[[which]]
}

# TODO:
# 1. Use a class?
# 2. Try to parse with col2rgb, and if it fails, try again with htmltools::parseCssColors()?
theme_create <- function(bg, fg, accent, qualitative, sequential) {
  theme <- list(
    bg = bg, fg = fg, accent = accent,
    qualitative = qualitative, sequential = sequential
  )
  lapply(theme, function(x) {
    if (identical(x, NA)) return(x)
    vapply(x, validate_color, character(1), USE.NAMES = FALSE)
  })
}

# x should be of length 1
validate_color <- function(x) {
  y <- tryCatch(
    col2rgb(x),
    error = function(e) {
      y <- htmltools::parseCssColors(x, mustWork = FALSE)
      if (is.na(y)) stop("Invalid color specification '", x, "'.", call. = FALSE)
      y
    }
  )
  if (is.character(y)) y else x
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
