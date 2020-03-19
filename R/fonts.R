# MAIN IDEA: Use the first font family that we know we can render,
# and if `auto_install = T`, try download->register->cache it
# from Google Fonts before trying to the next font
resolve_font_families <- function(families, auto_install) {
  if (!length(families)) return(families)

  for (i in seq_along(families)) {
    family <- families[[i]]
    # First, check if generally available to R
    if (can_render(family)) {
      break
    }
    # Next, register any cache with sysfonts/systemfonts
    if (has_gfont_cache(family)) {
      register_gfont_cache(family)
      throw_device_conditions(family)
      break
    }
    # Next, try downloading and registering
    if (isTRUE(auto_install)) {
      success <- try_gfont_download(family)
      if (success) {
        register_gfont_cache(family)
        message(
          "Font family '", family, "' successfully downloaded and registered."
        )
        throw_device_conditions(family)
        break
      }
    }
  }
  family
}

# Returns TRUE if we already have the capabilities to render
# the font (without any additional registration)
can_render <- function(family) {
  # If this family resolves to a path other than one of
  # the generic fonts, then consider this family available
  if (rlang::is_installed("ragg")) {
    f <- systemfonts::match_font(family)
    is_available <- !identical(f, systemfonts::match_font("sans")) &&
      !identical(f, systemfonts::match_font("serif")) &&
      !identical(f, systemfonts::match_font("mono"))
    return(is_available)
  }

  # If the family is generally *not* available, this code
  # should produce a warning with the font family name.
  # (BTW, if the family is not available as a system font,
  # but has been registered with sysfonts::font_add(), then
  # this will not throw a warning (which is good :)
  suppressWarnings(tryCatch(
    thematic_with_device(
      graphics::plot(1, family = family),
      device = grDevices::png
    ),
    warning = function(w) {
      !grepl(
        family,
        paste(w$message, collapse = "\n"),
        fixed = TRUE
      )
    }
  ))
}

has_gfont_cache <- function(family) {
  family_idx <- match(family, google_fonts$family)
  if (is.na(family_idx)) return(FALSE)
  font_info <- google_fonts[family_idx, ]
  dir.exists(font_cache_family(font_info$id))
}

throw_device_conditions <- function(family) {
  if (is_rstudio_device()) {
    message(
      "Rendering of auto-installed font families (e.g., ", family, ") ",
      "doesn't work at all with the RStudio graphics device. ",
      "Use `thematic_with_device()` to render these fonts correctly."
    )
  }
  if (rlang::is_installed("ragg")) return()
  if (rlang::is_installed("showtext")) return()
  warning(
    "Rendering of auto-installed font families (e.g., ", family, ") ",
    "requires either the showtext or ragg package to be installed. ",
    "Please install one of these packages, restart R, and call thematic_begin() again.",
    call. = FALSE
  )
}

try_gfont_download <- function(family) {
  family_idx <- match(family, google_fonts$family)
  if (is.na(family_idx)) {
    warning(call. = FALSE,
      "The font family '", family, "' doesn't appear to be available as a ",
      "Google Font. Try manually downloading and installing it on your system. ",
      "For more info, visit https://github.com/rstudio/thematic#fonts"
    )
    return(FALSE)
  }

  # Important: same target as has_gfont_cache()
  font_info <- google_fonts[family_idx, ]
  target <- font_cache_family(font_info$id)

  # Attempt download and register
  url <- paste0(
    file.path(gfont_url(), font_info$id),
    paste0("?download=zip&formats=", gfont_format)
  )
  tmpzip <- tempfile(fileext = ".zip")
  on.exit(unlink(tmpzip, recursive = TRUE), add = TRUE)
  tryCatch(
    {
      download.file(url, tmpzip)
      unzip(tmpzip, exdir = target)
      TRUE
    },
    error = function(e) {
      warning(
        "Failed to download the following Google Font family: ",
        family, call. = FALSE
      )
      FALSE
    }
  )

}

register_gfont_cache <- function(family) {
  family_idx <- match(family, google_fonts$family)
  if (is.na(family_idx)) return()
  font_info <- google_fonts[family_idx, ]

  # Return the path to a font family variant, if it exists
  font_file <- function(variant) {
    variant <- paste(
      sep = "-",
      font_info$id,
      font_info$version,
      font_info$defSubset,
      paste0(variant, ".", gfont_format)
    )
    variant <- file.path(font_cache_family(font_info$id), variant)
    if (file.exists(variant)) variant else NULL
  }

  files <- list()
  files$regular <- font_file("regular") %||% font_file("300")
  files$bold <- font_file("700") %||% files$regular
  files$italic <- font_file("italic") %||% font_file("300italic") %||% files$regular
  files$bolditalic <- font_file("700italic") %||% files$bold

  if (rlang::is_installed("systemfonts")) {
    systemfonts::register_font(
      family,
      files$regular,
      bold = files$bold,
      italic = files$italic,
      bolditalic = files$bolditalic
    )
  }

  if (rlang::is_installed("sysfonts")) {
    getFromNamespace("font_add", "sysfonts")(
      family,
      files$regular,
      bold = files$bold,
      italic = files$italic,
      bolditalic = files$bolditalic
    )
  }
}


gfont_url <- function() {
  getOption("gfonts.url", "https://google-webfonts-helper.herokuapp.com/api/fonts")
}

gfont_format <- "ttf"
