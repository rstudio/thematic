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
  Sys.getenv("THEMATIC_FONT_CACHE_DIR", path)
}


has_gfont_cache <- function(family) {
  font_info <- google_fonts[match(family, google_fonts$family), ]
  has_font_cache(font_info$id)
}

has_font_cache <- function(font_id) {
  dir.exists(font_cache(font_id))
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
  # TODO: worth doing?
  #if (has_package("extrafont")) {
  #  extrafont::font_import(
  #    paths = dirname(files$regular),
  #    prompt = FALSE
  #  )
  #}
}



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
    # TODO: show progress? Or at least spinning wheel?
    download.file(url, tmpzip)
    font_files <- unzip(tmpzip, exdir = target)
  }

  register_cache_gfont(font_info)
}

get_google_fonts <- function() {
  # TODO: remove dependency
  gfonts::get_all_fonts()
}

gfont_url <- function() {
  getOption("gfonts.url", "https://google-webfonts-helper.herokuapp.com/api/fonts")
}

gfont_format <- "ttf"
