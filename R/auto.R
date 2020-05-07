#' Set auto theming defaults
#'
#' Auto theming defaults are used to resolve `"auto"` values outside
#' of a **shiny** runtime (i.e., where auto theming might be based on
#' imperfect heuristics). Setting of auto defaults is especially useful
#' for developers of a custom rmarkdown output document that wish to
#' have more sensible auto theming behavior for users of the document.
#' In particular, by having the output document call `auto_defaults()`
#' "pre-knit" with the document's styling preferences (and restoring the
#' old defaults "post-knit"), users of the output document can then simply
#' call `thematic_on()` within their document to use those preferences.
#'
#' @details Call this function with no arguments to get the current auto defaults.
#'
#' @inheritParams thematic_on
#' @export
#' @examples
#' auto_defaults("black", "white")
#' thematic_on()
#' plot(1:10, 1:10)

auto_defaults <- function(bg = NULL, fg = NULL, accent = NULL, font = NULL) {
  cols <- dropNulls(list(bg = bg, fg = fg, accent = accent))
  defaults <- lapply(cols, function(x) {
    if (isTRUE(is.na(x))) x else parse_any_color(x)
  })
  if (!is.null(font)) {
    defaults$font <- as_font_spec(font)
  }
  oldPrefs <- .globals$auto_defaults
  if (length(defaults)) {
    .globals$auto_defaults <- defaults
  }
  invisible(oldPrefs)
}


resolve_auto_theme <- function() {
  theme <- .globals$theme
  outputInfo <- shiny_output_info()
  autoDefaults <- auto_defaults()
  bsThemeColors <- bs_theme_colors()
  rsThemeColors <- rs_theme_colors()

  # Resolve auto colors, if relevant
  for (col in c("bg", "fg", "accent")) {
    if (!is_auto(theme[[col]])) {
      next
    }
    # shiny::getCurrentOutputInfo() gets 1st priority, since its
    # *output* level styles, whereas autoDefaults are intended for
    # the *document* level (i.e., defaults for a custom rmarkdown
    # format). Perhaps there are situations where shiny::getCurrentOutputInfo()
    # doesn't give you quite what you want, but in that case, you should be
    # styling the containing div with your desired styles!
    #
    # Also, importantly, bsThemeColors contains Bootstrap Sass info
    # only _if_ a knit is in progress (we don't necessary want to use
    # a bs_theme_get_variable() info unless we know it's going to be
    # relevant for the final output)
    theme[[col]] <- outputInfo[[col]] %||%
      autoDefaults[[col]] %||%
      bsThemeColors[[col]] %||%
      rsThemeColors[[col]] %||%
      theme[[col]]
    if (isTRUE("auto" == theme[[col]])) {
      maybe_warn(
        "thematic was unable to resolve `", col, "='auto'`. ",
        "Try providing an actual color (or `NA`) to the `", col, "` argument of `thematic_on()`. ",
        "By the way, 'auto' is only officially supported in `shiny::renderPlot()`, ",
        "some rmarkdown scenarios (specifically, `html_document()` with `theme!=NULL`), ",
        "in RStudio, or if `auto_defaults()` is set.",
        id = paste0("auto-detection-failure-", col)
      )
      theme[[col]] <- switch(col, bg = "white", fg = "black", NA)
    } else {
      theme[[col]] <- parse_any_color(theme[[col]])
    }

    # Retain auto class so that the _next_ time this hook
    # gets called we know to resolve the value again
    # (e.g., renderPlot() executes again, but this time with different styling)
    theme[[col]] <- as_auto(theme[[col]])
  }

  # resolve sequential
  theme$sequential <- if (is.function(theme$sequential_func)) {
     do.call(theme$sequential_func, theme)
  } else {
    theme$sequential_func
  }

  # Make sure we can parse any non-missing colors
  for (col in c("bg", "fg", "accent", "qualitative", "sequential")) {
    if (isTRUE(is.na(theme[[col]]))) next
    val <- vapply(theme[[col]], parse_any_color, character(1), USE.NAMES = FALSE)
    # Retain auto class (see comment above)
    theme[[col]] <- if (is_auto(theme[[col]])) as_auto(val) else val
  }

  if (any(vapply(theme$font, is_auto, logical(1)))) {
    # Note how this matches the order of priority for colors, as well
    spec <- shiny_font_spec(outputInfo$font) %||%
      autoDefaults$font %||%
      bs_font_spec() %||%
      rs_font_spec() %||%
      font_spec()

    # As with colors above, make sure to retain the auto class so that the
    # _next_ time this hook gets called we know to resolve the value again
    for (key in names(spec)) {
      if (is_auto(theme$font[[key]])) {
        theme$font[[key]] <- as_auto(spec[[key]])
      }
    }

  } else {
    theme$font <- as_font_spec(theme$font)
  }

  .globals$theme <- theme

  invisible()
}

# ------------------------------------------------------------
# Colors
# ------------------------------------------------------------

shiny_output_info <- function() {
  if (!is_installed("shiny")) return(NULL)
  info <- tryNULL(shiny::getCurrentOutputInfo())
  # Return early if we're not in any output context
  if (is.null(info)) return(NULL)
  # Return early with a message to update shiny if the relevant
  # info isn't populated
  nms <- c("bg", "fg", "accent", "font")
  missing <- setdiff(nms, names(info))
  if (length(missing)) {
    maybe_warn(
      "Auto-theming with shiny requires v1.4.0.9900 or higher",
      id = "upgrade-shiny"
    )
    return(NULL)
  }
  # This is what I get for announcing before shiny was ready
  res <- lapply(info[nms], function(x) {
    if (!shiny::is.reactive(x)) {
      stop(
        "Expected shiny::getCurrentOutputInfo() to return reactive expressions. ",
        "Try upgrading shiny: remotes::install_github('rstudio/shiny#2740')"
      )
    }
    x()
  })
  rlang::set_names(res, nms)
}

bs_theme_colors <- function() {
  if (!in_html_document()) return(NULL)

  cols <- if ("3" %in% bootstraplib::theme_version()) {
    bootstraplib::bs_theme_get_variables(c("body-bg", "text-color", "link-color"))
  } else {
    bootstraplib::bs_theme_get_variables(c("body-bg", "body-color", "link-color"))
  }

  rlang::set_names(cols, c("bg", "fg", "accent"))
}


rs_theme_colors <- function() {
  if (!is_rstudio()) return(NULL)
  # Only apply RStudio theme if the device is (or will be) RStudio
  if (!"RStudioGD" %in% infer_device()) return(NULL)

  # Hopefully someday this'll return font/accent info
  # https://github.com/rstudio/rstudioapi/issues/174
  info <- getThemeInfo()

  # These colors were taken manually from the theme preview
  # (they are the token color)
  info$accent <- switch(
    info$editor,
    Ambiance = "#CFB171",
    Chaos = "#27759B",
    Chrome = "#98108D",
    Clouds = "#98108D",
    `Clouds Midnight` = "#9A8767",
    Cobalt = "#F7A600",
    `Crimson Editor` = "#2918FF",
    Dawn = "#80523F",
    Dracula = "#F785CE",
    Dreamweaver = "#2918FF",
    Eclipse = "#86058D",
    `Idle Fingers` = "#CB8334",
    Katzenmilch = "#7255B3",
    `Kr Theme` = "#9FA695",
    Material = "#DC9D67",
    Merbivore = "#F37900",
    `Merbivore Soft` = "#F48A3B",
    `Mono Industrial` = "#ABA86D",
    Monokai = "#F0297D",
    `Pastel On Dark` = "#8486DF",
    `Solarized Dark` = "#90A300",
    `Solarized Light` = "#90A300",
    Textmate = "#2918FF",
    Tomorrow = "#9265B3",
    `Tomorrow Night` = "#B99FC4",
    `Tomorrow Night 80s` = "#CFA4D3",
    `Tomorrow Night Blue` = "#EBC3FF",
    `Tomorrow Night Bright` = "#C8A2DE",
    Twilight = "#CFB171",
    `Vibrant Ink` = "#F57000",
    Xcode = "#C706AF",
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
  if (!length(font)) return(NULL)
  font_spec(
    font$families,
    scale = size_to_scale(font$size)
  )
}

bs_font_spec <- function(){
  if (!in_html_document()) return(NULL)

  family <- bootstraplib::bs_theme_get_variables("font-family-base")
  families <- strsplit(gsub('"', '', family), ", ")[[1]]
  size <- bootstraplib::bs_theme_get_variables("font-size-base")
  font_spec(families, scale = size_to_scale(size))
}

in_html_document <- function() {
  if (!getOption("knitr.in.progress", FALSE)) return(FALSE)
  if (!is_installed("bootstraplib")) return(FALSE)
  !is.null(bootstraplib::bs_theme_get())
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
      maybe_warn(
        "CSS font-size unit of '", size, "' not supported by thematic.",
        id = "unsupported-font-size-unit"
      )
      1
    }
  )
}


rs_font_spec <- function() {
  if (!is_rstudio()) return(NULL)

  # readRStudioPreference was introduced in RStudio 1.3
  pts <- tryCatch(readRStudioPreference("font_size_points"), error = function(e) 12)

  # Note that server_editor_font appears in RStudio 1.4
  # so it'll take awhile for this to be widely supported
  family <- tryCatch(readRStudioPreference("server_editor_font"), error = function(e) "")

  if (isTRUE(nzchar(family))) {
    # TODO: respect global device pointsize
    return(font_spec(family, scale = pts / 12))
  }

  if (identical(versionInfo()$mode, "server")) {
    message("Auto font detection in RStudio Server requires RStudio 1.4 or higher.")
    return(NULL)
  }

  # Try and read RStudio Desktop editor font preference
  # https://support.rstudio.com/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State
  family <- grep(
    '\\s*"?font.*fixedWidth\\s*=',
    tryNULL(rstudio_desktop_prefs()),
    value = TRUE
  )
  # TODO: if font hasn't been specified, this font.fixedWidth wont appear...
  # is it correct to just use the default font?
  if (!length(family)) {
    return(NULL)
  }
  family <- strsplit(family, "=")[[1]][2]
  family <- sub('^\\s*"?', '', sub('"?\\s*;?\\s*$', '', family))
  # TODO: respect global device pointsize
  font_spec(family, scale = pts / 12)
}

rstudio_desktop_prefs <- function() {
  if (.Platform$OS.type == "windows") {
    return(readLines(
      file.path(Sys.getenv("APPDATA"), "RStudio", "desktop.ini")
    ))
  }
  sys <- Sys.info()[["sysname"]]
  if (sys == "Darwin") {
    return(
      system("defaults read com.rstudio.desktop", intern = TRUE)
    )
  }
  readLines("~/.config/RStudio/desktop.ini")
}


tag_auto <- function(x) {
  if (identical(x, "auto")) as_auto(x) else x
}

as_auto <- function(x) {
  oldClass(x) <- c("thematic_auto", oldClass(x))
  x
}

is_auto <- function(x) {
  inherits(x, "thematic_auto")
}
