#' Configure auto theming behavior
#'
#' Auto theming is really only "guaranteed" to work inside of a **shiny**
#' runtime. In any other context, auto theming is based on a set of heuristics,
#' which won't fit every use case. As a workaround, this function allows one
#' to configure both a preference for specific auto values (e.g., `bg`, `fg`, etc)
#' as well as the `priority` that certain information should receive.
#'
#' @details Configuring auto theming behavior is especially useful
#' for developers of a custom rmarkdown output document that wish to
#' have more sensible auto theming behavior for users of the document.
#' In particular, by having the output document call `auto_config_set()`
#' "pre-knit" with the document's styling preferences (and restoring the
#' old defaults "post-knit"), users of the output document can then simply
#' call `thematic_on()` within their document to use those preferences.
#'
#' @details Call this function with no arguments to get the current auto defaults.
#'
#' @inheritParams thematic_on
#' @param priority the order of priority to use when resolving auto values.
#' Possible values include:
#'   * `"shiny"`: use [shiny::getCurrentOutputInfo()] values (if any) to resolve auto values.
#'   * `"config"`: use the values provided to this function (if any) to resolve auto values.
#'   * `"bootstraplib"`: use [bootstraplib::bs_theme_get_variables()] values (if any)
#'     to resolve auto values (only relevant when knitr is in progress).
#'   * `"rstudio"`: use [rstudioapi::getThemeInfo()] values (if any) to resolve auto values.
#'
#' @rdname auto-config
#' @export
#' @examples
#' old_config <- auto_config_set(auto_config("black", "white"))
#' thematic_with_theme(
#'   thematic_theme(), {
#'     plot(1:10, 1:10)
#'  })
#' auto_config_set(old_config)
auto_config <- function(bg = NULL, fg = NULL, accent = NULL, font = NULL,
                        priority = c("shiny", "config", "bootstraplib", "rstudio")) {
  cols <- dropNulls(list(bg = bg, fg = fg, accent = accent))
  config <- lapply(cols, function(x) {
    if (isTRUE(is.na(x))) x else parse_any_color(x)
  })
  if (!is.null(font)) {
    config$font <- as_font_spec(font)
  }
  priority <- priority %||% priorities()
  config$priority <- match.arg(priority, priorities(), several.ok = TRUE)
  structure(config, class = "thematic_auto_config")
}

#' @rdname auto-config
#' @param config a `auto_config()` object.
#' @export
auto_config_set <- function(config) {
  if (!inherits(config, "thematic_auto_config")) {
    stop("`config` must be a `auto_config()` object", call. = FALSE)
  }
  old_config <- auto_config_get()
  .globals$auto_config <- config
  invisible(old_config)
}

#' @rdname auto-config
#' @export
auto_config_get <- function() {
  # This should contain priority even if a config hasn't been set
  config <- utils::modifyList(auto_config(), .globals$auto_config %||% list())
  structure(config, class = "thematic_auto_config")
}

priorities <- function() {
  c("shiny", "config", "bootstraplib", "rstudio")
}

#' Resolve auto values
#'
#' Resolves `'auto'` values based on the current execution environment
#' and configuration (i.e., [auto_config_get()]).
#'
#' @return The `theme` object with resolved `'auto'` values.
#'
#' @param theme a `thematic_theme()` object.
#' @export
#' @seealso [auto_config_set()]
#' @examples
#'
#' old_config <- auto_config_set(auto_config(bg = "black", fg = "white"))
#'
#' # Resolving auto values in local theme objects
#' theme <- thematic_theme()
#' theme[c("bg", "fg")]
#' theme <- auto_resolve_theme(theme)
#' theme[c("bg", "fg")]
#'
#' # By default, auto values are resolved when accessing
#' # global theme options
#' thematic_on()
#' thematic_get_option("bg", resolve = FALSE)
#' thematic_get_option("bg")
#' thematic_off()
#'
#' auto_config_set(old_config)
#'
auto_resolve_theme <- function(theme) {
  if (length(theme) == 0) {
    return(theme)
  }
  if (!is_thematic_theme(theme)) {
    stop("`theme` must be a `thematic_theme()` object", call. = FALSE)
  }

  auto_color_info <- lapply(auto_config_get()$priority, function(x) {
    switch(
      x,
      shiny = shiny_output_info(),
      config = auto_config_get(),
      bootstraplib = bs_theme_colors(),
      rstudio = rs_theme_colors(),
      stop("`priority` of '", x, "' is not implemented", call. = FALSE)
    )
  })

  # Resolve auto colors, if relevant
  for (col in c("bg", "fg", "accent")) {
    if (!is_auto(theme[[col]])) {
      next
    }
    theme[[col]] <- Reduce(`%||%`, lapply(auto_color_info, `[[`, col)) %||% theme[[col]]
    if (isTRUE("auto" == theme[[col]])) {
      maybe_warn(
        "thematic was unable to resolve `", col, "='auto'`. ",
        "Try providing an actual color (or `NA`) to the `", col, "` argument of `thematic_on()`. ",
        "By the way, 'auto' is only officially supported in `shiny::renderPlot()`, ",
        "some rmarkdown scenarios (specifically, `html_document()` with `theme!=NULL`), ",
        "in RStudio, or if `auto_config_set()` is used.",
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

  # resolve sequential, if necessary
  sequential_func <- attr(theme$sequential, "sequential_func")
  if (is.function(sequential_func)) {
    theme$sequential <- do.call(sequential_func, theme[c("bg", "fg", "accent")])
  }

  # Make sure we can parse any non-missing colors
  for (col in c("bg", "fg", "accent", "qualitative", "sequential")) {
    if (isTRUE(is.na(theme[[col]]))) next
    val <- vapply(theme[[col]], parse_any_color, character(1), USE.NAMES = FALSE)
    # Retain auto class (see comment above)
    theme[[col]] <- if (is_auto(theme[[col]])) as_auto(val) else val
  }

  # Retain the function that created sequential codes, if necessary
  if (is.function(sequential_func)) {
    theme$sequential <- structure(theme$sequential, sequential_func = sequential_func)
  }

  # Resolve fonts
  if (any(vapply(theme$font, is_auto, logical(1)))) {
    # Note the similarity to resolution of auto colors
    auto_font_info <- lapply(auto_config_get()$priority, function(x) {
      switch(
        x,
        shiny = shiny_font_spec(shiny_output_info()$font),
        config = auto_config_get()$font,
        bootstraplib = bs_font_spec(),
        rstudio = rs_font_spec(),
        stop("`priority` of '", x, "' is not implemented", call. = FALSE)
      )
    })
    spec <- Reduce(`%||%`, auto_font_info) %||% font_spec()

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

  theme
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
  if (is_auto(x)) return(x)
  oldClass(x) <- c("thematic_auto", oldClass(x))
  x
}

is_auto <- function(x) {
  inherits(x, "thematic_auto")
}
