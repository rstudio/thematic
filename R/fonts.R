resolve_family <- function(family) {
  # What font will this specified family resolve to (OS dependent)?
  font <- systemfonts::match_font(family)
  # Get the _resolved_ family name...requires dev version of systemfonts
  # (could do something like `font$family %in% systemfonts::system_fonts()$family`
  # with CRAN version, but that's way more expensive)
  info <- systemfonts::font_info(font$family, path = font$path, index = font$index)
  info$family
}


# if font is not already in the cache, this downloads font files
# from Google Fonts
download_google_font <- function(family) {
  font_info <- google_fonts[match(family, google_fonts$family), ]

  target <- font_cache(font_info$id)
  # Has this family already been downloaded?
  if (dir.exists(target))  {
    font_files <- list.dirs(target)
  } else {
    url <- paste0(
      file.path(gfont_url(), font_info$id),
      paste0("?download=zip&formats=", gfont_format)
    )
    tmpzip <- tempfile(fileext = ".zip")
    download.file(url, tmpzip)
    #curl::curl_download(url, tmpzip)
    font_files <- unzip(tmpzip, exdir = target)
  }

  register_google_font(font_info)
}

register_google_fonts <- function() {
  font_ids <- dir(font_cache())
  font_info <- google_fonts[google_fonts$id %in% font_ids, ]

  for (i in seq_len(nrow(font_info))) {
    register_google_font(font_info[i, ])
  }
}

register_google_font <- function(info) {
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

  systemfonts::register_font(
    info$family,
    files$regular,
    bold = files$bold,
    italic = files$italic,
    bolditalic = files$bolditalic
  )

  if (has_package("sysfonts")) {
    getFromNamespace("font_add", "sysfonts")(
      info$family,
      files$regular,
      bold = files$bold,
      italic = files$italic,
      bolditalic = files$bolditalic
    )
  }
}

has_google_font <- function(family) {
  font_info <- google_fonts[match(family, google_fonts$family), ]
  dir.exists(font_cache(font_info$id))
}

font_cache <- function(...) {
  cache_path(file.path("fonts", ...))
}

gfont_url <- function() {
  getOption("gfonts.url", "https://google-webfonts-helper.herokuapp.com/api/fonts")
}

gfont_format <- "ttf"
