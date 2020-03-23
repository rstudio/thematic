try_gfont_download_and_register <- function(info = NULL, family) {
  if (is.null(info)) {
    warning(call. = FALSE,
      "The font family '", family, "' doesn't appear to be available as a ",
      "Google Font. Try manually downloading and installing it on your system. ",
      "For more info, visit https://github.com/rstudio/thematic#fonts"
    )
    return(FALSE)
  }

  # Attempt to download
  url <- paste0(
    file.path(gfont_url(), info$id),
    paste0("?download=zip&formats=", gfont_format)
  )
  tmpzip <- tempfile(fileext = ".zip")
  on.exit(unlink(tmpzip, recursive = TRUE), add = TRUE)
  res <- try({
    download.file(url, tmpzip)
    unzip(tmpzip, exdir = font_cache_family(info$id))
    register_gfont_cache(info)
  })
  success <- !inherits(res, "try-error")
  if (success) {
    message("Successfully downloaded the '", family, "' font family.")
  } else {
    warning(
      "Failed to download the '", family, "' font family.",
      call. = FALSE
    )
  }
  success
}

register_gfont_cache <- function(info = NULL) {
  if (is.null(info)) return()

  cache_path <- font_cache_family(info$id)
  # Return the path to a font family variant, if it exists
  font_file <- function(variant) {
    variant <- paste(
      sep = "-",
      info$id,
      info$version,
      info$defSubset,
      paste0(variant, ".", gfont_format)
    )
    variant <- file.path(cache_path, variant)
    if (file.exists(variant)) variant else NULL
  }

  files <- list()
  files$regular <- font_file("regular") %||% font_file("300")
  files$bold <- font_file("700") %||% files$regular
  files$italic <- font_file("italic") %||% font_file("300italic") %||% files$regular
  files$bolditalic <- font_file("700italic") %||% files$bold

  if (sum(lengths(files)) == 0) return()

  try(
    systemfonts::register_font(
      info$family,
      files$regular,
      bold = files$bold,
      italic = files$italic,
      bolditalic = files$bolditalic
    )
  )

  try(
    getFromNamespace("font_add", "sysfonts")(
      info$family,
      files$regular,
      bold = files$bold,
      italic = files$italic,
      bolditalic = files$bolditalic
    )
  )
}


gfont_info <- function(family) {
  gfonts <- get_google_fonts()
  family_idx <- match(family, gfonts$family)
  if (is.na(family_idx)) {
    NULL
  } else {
    gfonts[family_idx, , drop = TRUE]
  }
}

has_gfont_cache <- function(info) {
  if (is.null(info)) return(FALSE)

  font_files <- dir(
    font_cache_family(info$id),
    pattern = paste0(".", gfont_format, "$")
  )
  length(font_files) >= 1
}

# Use updated version of fonts, if available; otherwise,
# fallback to the set shipped with the package
get_google_fonts <- function() {
  .globals$google_fonts %||% google_fonts
}

gfont_url <- function() {
  getOption("gfonts.url", "https://google-webfonts-helper.herokuapp.com/api/fonts")
}

gfont_format <- "ttf"
