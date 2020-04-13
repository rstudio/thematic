#' Set auto theming preferences
#'
#' Setting of auto-theming preferences is primarily useful for developers
#' of a custom rmarkdown format that wish to have better default auto-theming
#' behavior. By having the output document call `auto_preferences_set()`
#' "pre-knit" (and `auto_preferences_clear()` "post-knit"), users of the
#' output document can then simply call `thematic_begin()` within their document
#' to adopt these preferences.
#'
#' @inheritParams thematic_begin
#' @rdname auto_preferences
#' @export
#' @examples
#' auto_preferences_set("black", "white")
#' thematic_begin()
#' plot(1:10, 1:10)

auto_preferences_set <- function(bg = NULL, fg = NULL, accent = NULL, font = NULL) {
  cols <- dropNulls(list(bg = bg, fg = fg, accent = accent))
  preferences <- lapply(cols, function(x) {
    if (isTRUE(is.na(x))) x else parse_any_color(x)
  })
  if (!is.null(font)) {
    preferences$font <- if (is_font_spec(font)) font else font_spec(font)
  }
  oldPrefs <- .globals$auto_preferences
  .globals$auto_preferences <- preferences
  invisible(oldPrefs)
}

#' @rdname auto_preferences
#' @export
auto_preferences_get <- function() {
  .globals$auto_preferences
}


resolve_auto_theme <- function() {
  theme <- .globals$theme
  outputInfo <- shiny_output_info()
  autoPreferences <- auto_preferences_get()
  bsThemeColors <- bs_theme_colors()
  rsThemeColors <- rs_theme_colors()

  # Resolve auto colors, if relevant
  for (col in c("bg", "fg", "accent")) {
    if (!identical(theme[[col]], "auto")) {
      next
    }
    theme[[col]] <- outputInfo[[col]] %||%
      autoPreferences[[col]] %||%
      bsThemeColors[[col]] %||%
      rsThemeColors[[col]] %||%
      theme[[col]]
    if (identical(theme[[col]], "auto")) {
      # TODO: provide an option to suppress?
      warning(
        "thematic was unable to resolve `", col, "='auto'`. ",
        "Try providing an actual color (or `NA`) to the `", col, "` argument of `thematic_begin()`. ",
        "By the way, 'auto' is only officially supported in `shiny::renderPlot()`, ",
        "some rmarkdown scenarios (specifically, `html_document()` with `theme!=NULL`), ",
        "in RStudio, or if `auto_preferences_set()` is set.",
        call. = FALSE
      )
      theme[[col]] <- NA
    } else {
      theme[[col]] <- htmltools::parseCssColors(theme[[col]])
    }
  }

  # Construct the sequential palette
  if (inherits(theme$sequential, "thematic_sequential_options")) {
    theme$sequential <- resolve_sequential_gradient(
      fg = theme$fg, accent = theme$accent, bg = theme$bg,
      options = theme$sequential
    )
  }

  # Make sure we can parse any non-missing colors
  for (col in c("bg", "fg", "accent", "qualitative", "sequential")) {
    if (isTRUE(is.na(theme[[col]]))) next
    theme[[col]] <- vapply(theme[[col]], parse_any_color, character(1), USE.NAMES = FALSE)
  }

  # Make sure font is a sensible value
  if (isTRUE(is.na(theme$font))) {
    theme$font <- font_spec()
  }

  if (is_font_spec(theme$font)) {
    .globals$theme <- theme
    return()
  }

  if (!identical(theme$font, "auto")) {
    stop("The `font` argument must be either `NA`, `'auto'`, or a `font_spec()` object", call. = FALSE)
  }

  # Resolve auto fonts
  if (length(outputInfo$font)) {
    theme$font <- shiny_font_spec(outputInfo$font)
  } else {
    theme$font <- bs_font_spec() %||% font_spec()
  }

  .globals$theme <- theme

  invisible()
}

# ------------------------------------------------------------
# Colors
# ------------------------------------------------------------

shiny_output_info <- function() {
  if (!is_installed("shiny")) return(NULL)
  tryNULL(shiny::getCurrentOutputInfo())
}

bs_theme_colors <- function() {
  if (!is_installed("bootstraplib")) return(NULL)
  theme <- bootstraplib::bs_theme_get()
  if (is.null(theme)) return(NULL)

  cols <- if ("3" %in% bootstraplib::theme_version(theme)) {
    bootstraplib::bs_theme_get_variables(c("body-bg", "text-color", "link-color"))
  } else {
    bootstraplib::bs_theme_get_variables(c("body-bg", "body-color", "link-color"))
  }

  rlang::set_names(cols, c("bg", "fg", "accent"))
}


rs_theme_colors <- function() {
  if (!is_installed("rstudioapi")) return(NULL)
  if (!is_installed("htmltools")) return(NULL)

  # Maybe someday this'll return font/accent info
  # https://github.com/rstudio/rstudioapi/issues/174
  info <- tryNULL(rstudioapi::getThemeInfo())
  if (is.null(info)) return(NULL)

  # TODO: add more editors
  info$accent <- switch(
    info$editor,
    `Tomorrow Night 80s` = "#CFA4D3",
    NA
  )
  rlang::set_names(
    info[c("background", "foreground", "accent")],
    c("bg", "fg", "accent")
  )
}


# ------------------------------------------------------------
# Fonts
# ------------------------------------------------------------

shiny_font_spec <- function(font) {
  if (isTRUE(font$renderedFamily %in% generic_css_families())) {
    warning(
      "renderPlot()'s autoTheme doesn't support generic CSS font families (e.g. '",
      font$renderedFamily, "'). Consider using a Google Font family instead ",
      "https://fonts.google.com/", call. = FALSE
    )
    font$renderedFamily <- NULL
  }
  families <- as.character(font$renderedFamily %||% font$families)
  font_spec(
    setdiff(families, generic_css_families()) %||% "",
    scale = size_to_scale(font$size)
  )
}

bs_font_spec <- function(){
  if (!is_installed("bootstraplib")) return(NULL)
  theme <- bootstraplib::bs_theme_get()
  if (is.null(theme)) return(NULL)

  family <- bootstraplib::bs_theme_get_variables("font-family-base")
  families <- strsplit(gsub('"', '', family), ", ")[[1]]
  size <- bootstraplib::bs_theme_get_variables("font-size-base")
  font_spec(
    setdiff(families, generic_css_families()) %||% "",
    scale = size_to_scale(size)
  )
}

# Translate CSS font-size to font_spec(scale = ...)
# https://developer.mozilla.org/en-US/docs/Web/CSS/length
size_to_scale <- function(size, pointsize = 12) {
  if (length(size) != 1) {
    stop("Expect font size to be of length 1.", call. = FALSE)
  }

  size <- sub("^\\s+", "", sub("\\s+$", "", size))

  # Based on https://stackoverflow.com/a/5912657/1583084
  size <- switch(
    size,
    `xx-small` = "50%",
    `x-small`  = "62.5%",
    small      = ,
    smaller    = "80%",
    medium     = "100%",
    large      = ,
    larger     = "112.5%",
    `x-large`  = "150%",
    `xx-large` = "200%",
    size
  )

  # Translate some important relative units (1rem is the default for BS4).
  # Yes, these will be wrong if the reference size is different from
  # the default of 1(r)em = 16px = 12pt...shrug
  if (grepl("[0-9]+r?em$", size)) {
    return(as.numeric(sub("r?em$", "", size)))
  }
  if (grepl("[0-9]+%", size)) {
    return(as.numeric(sub("%$", "", size)) / 100)
  }

  # ------------------------------------------------------
  # Translate all absolute units to inches
  # https://developer.mozilla.org/en-US/docs/Web/CSS/length
  # ------------------------------------------------------
  from_to <- function(x, from, to, factor) {
    pattern <- paste0("[0-9]+", from, "$")
    if (!grepl(from, x)) return(x)
    paste0(as.numeric(sub(from, "", x)) * factor, to)
  }

  size <- from_to(size, "px", "in", 1/96)
  size <- from_to(size, "cm", "in", 2.54)
  size <- from_to(size, "mm", "in", 25.4)
  size <- from_to(size, "pc", "in", 1/6)
  pts <- from_to(size, "in", "pt", 72)

  tryCatch(
    as.numeric(sub("pt", "", pts)) / (.globals$device$args$pointsize %||% 12),
    warning = function() {
      warning(
        "CSS font-size unit of '", size, "' not supported by thematic.",
        call. = FALSE
      )
      1
    }
  )
}


# https://drafts.csswg.org/css-fonts-4/#generic-font-families
generic_css_families <- function() {
  c(
    "serif", "sans-serif", "cursive", "fantasy", "monospace",
    "system-ui", "emoji", "math", "fangsong",
    "ui-serif", "ui-sans-serif", "ui-monospace", "ui-rounded",
    # not part of the official spec (earlier versions of system-ui)
    "-apple-system" , "BlinkMacSystemFont"
  )
}
