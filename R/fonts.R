#' File path to thematic's font cache
#'
#' Set the absolute path to be used for managing thematic's font caching.
#'
#' @rdname font_cache
#' @export
font_cache_dir_get <- function() {
  Sys.getenv(
    "THEMATIC_FONT_CACHE_DIR",
    file.path(cache_dir(), "thematic_font_cache")
  )
}

#' @rdname font_cache
#' @export
font_cache_dir_set <- function(path) {
  # TODO: if users can set/add this, we need probably need to export a registration function?
  Sys.getenv("THEMATIC_FONT_CACHE_DIR", path)
}

font_cache_clear <- function() {
  unlink(font_cache_dir_get(), recursive = TRUE)
}

font_cache <- function(font_id = NULL) {
  cache_dir <- font_cache_dir_get()
  if (is.null(font_id)) {
    dir(cache_dir, full.names = TRUE)
  } else {
    file.path(cache_dir, font_id)
  }
}


register_cache_gfonts <- function() {
  font_ids <- basename(font_cache())
  font_info <- google_fonts[google_fonts$id %in% font_ids, ]

  for (i in seq_len(nrow(font_info))) {
    register_cache_gfont(font_info[i, ])
  }
}

register_cache_gfont <- function(info) {
  # Return the path to a font family variant, if it exists
  font_variant <- function(variant) {
    variant <- paste(
      sep = "-",
      info$id,
      info$version,
      info$defSubset,
      paste0(variant, ".", gfont_format)
    )
    variant <- file.path(font_cache(info$id), variant)
    if (file.exists(variant)) variant else NULL
  }

  files <- list()
  files$regular <- font_variant("regular") %||% font_variant("300")
  files$bold <- font_variant("700") %||% files$regular
  files$italic <- font_variant("italic") %||% font_variant("300italic") %||% files$regular
  files$bolditalic <- font_variant("700italic") %||% files$bold

  if (rlang::is_installed("systemfonts")) {
    systemfonts::register_font(
      info$family,
      files$regular,
      bold = files$bold,
      italic = files$italic,
      bolditalic = files$bolditalic
    )
  }

  if (rlang::is_installed("sysfonts")) {
    getFromNamespace("font_add", "sysfonts")(
      info$family,
      files$regular,
      bold = files$bold,
      italic = files$italic,
      bolditalic = files$bolditalic
    )
  }

}


resolve_font_families <- function(families, auto_install) {
  if (!length(families)) return(families)

  for (i in seq_along(families)) {
    family <- families[[i]]
    if (is_registered(family)) {
      break
    }
    if (isTRUE(auto_install)) {
      success <- try_download_and_register_gfont(family)
      if (success) {
        # Auto-installed fonts are registered via systemfonts (& sysfonts),
        # which guarantees that they'll work with ragg (& other devices, thanks
        # to showtext), but not necessarily other devices (especially RStudioGD)
        if (is_rstudio_device()) {
          message(
            "If you encounter font rendering issues, ",
            "try wrapping your plotting code in `thematic_with_device()` ",
            "and/or installing the showtext package."
          )
        }

        break
      }
    }
  }

  family
}

is_registered <- function(family) {
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
        font$family,
        paste(w$message, collapse = "\n"),
        fixed = TRUE
      )
    }
  ))
}


try_download_and_register_gfont <- function(family) {
  family_idx <- match(family, google_fonts$family)
  if (is.na(family_idx)) {
    warning(call. = FALSE,
      "The font family '", family, "' doesn't appear to be available as a ",
      "Google Font. Try manually downloading and installing it on your system. ",
      "For more info, visit https://github.com/rstudio/thematic#fonts"
    )
    return(FALSE)
  }

  # Check for cache hit
  font_info <- google_fonts[family_idx, ]
  target <- font_cache(font_info$id)
  if (dir.exists(target)) {
    return(TRUE)
  }

  # Attempt download and register
  url <- paste0(
    file.path(gfont_url(), font_info$id),
    paste0("?download=zip&formats=", gfont_format)
  )
  tmpzip <- tempfile(fileext = ".zip")
  on.exit(unlink(tmpzip, recursive = TRUE), add = TRUE)
  res <- tryCatch(
    # TODO: show progress? Or at least spinning wheel?
    download.file(url, tmpzip),
    error = function(e) {
      warning(
        "Failed to download and register the following Google Font family: '",
        family, "'. ", call. = FALSE
      )
      FALSE
    },
    finally = {
      unzip(tmpzip, exdir = target)
      register_cache_gfont(font_info)
    }
  )
}

gfont_url <- function() {
  getOption("gfonts.url", "https://google-webfonts-helper.herokuapp.com/api/fonts")
}

gfont_format <- "ttf"
