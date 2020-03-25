try_gfont_download_and_register <- function(family) {
  # If no match in Google Font cache, look for a match with an updated set
  family_idx <- match(family, google_fonts$family)
  if (is.na(family_idx)) {
    google_fonts <- try_fetch_google_fonts()
    family_idx <- match(family, google_fonts$family)
    if (is.na(family_idx)) {
      warning(
        "The font family '", family, "' doesn't appear to be available as a ",
        "Google Font. Try manually downloading and installing it on your system. ",
        "For more info, visit https://github.com/rstudio/thematic#fonts",
        call. = FALSE
      )
      return(FALSE)
    }
  }

  # Attempt to download
  url <- paste0(
    file.path(gfont_url(), gfont_id(family)),
    paste0("?download=zip&formats=", gfont_format)
  )
  tmpzip <- tempfile(fileext = ".zip")
  on.exit(unlink(tmpzip, recursive = TRUE), add = TRUE)
  res <- try({
    download.file(url, tmpzip)
    unzip(tmpzip, exdir = gfont_cache_dir(family))
    try_register_gfont_cache(family)
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


try_register_gfont_cache <- function(family) {

  cache_dir <- gfont_cache_dir(family)

  # Return the path to a font family variant, if it exists
  # NOTE: this does assume the google fonts service (i.e., gfonts_url())
  # includes in the variant in the suffix of the filename
  font_file <- function(variant) {
    dir(
      cache_dir, full.names = TRUE,
      pattern = paste0(variant, "\\.", gfont_format, "$")
    )
  }

  # Nothing to do if we can't find a regular font file definition
  regular <- font_file("regular") %||% font_file("300")
  if (!length(regular)) return()

  bold <- font_file("700") %||% regular
  italic <- font_file("italic") %||% font_file("300italic") %||% regular
  bolditalic <- font_file("700italic") %||% bold

  try(
    systemfonts::register_font(
      family, regular,
      bold = bold,
      italic = italic,
      bolditalic = bolditalic
    )
  )

  try(
    getFromNamespace("font_add", "sysfonts")(
      family, regular,
      bold = bold,
      italic = italic,
      bolditalic = bolditalic
    )
  )
}


try_fetch_google_fonts <- function() {
  if (!is_installed("curl")) return(google_fonts)
  if (!is_installed("jsonlite")) return(google_fonts)
  if (!curl::has_internet()) return(google_fonts)
  new_length <- tryCatch(get_content_length(), error = function(e) NA)
  old_length <- attr(google_fonts, "content-length")
  if (identical(new_length, old_length)) {
    return(google_fonts)
  }
  tryCatch(get_google_fonts(), error = function(e) google_fonts)
}

get_content_length <- function() {
  content_length(curl::curl_fetch_memory(
    gfont_metadata_url(),
    handle = curl::new_handle(nobody = TRUE)
  ))
}

content_length <- function(res) {
  headers <- rawToChar(res$headers)
  m <- regexec("Content-Length:\\s+([0-9]+)", headers)
  regmatches(headers, m)[[1]][2]
}

get_google_fonts <- function() {
  res <- curl::curl_fetch_memory(gfont_url())
  jsonlite::parse_json(rawToChar(res$content), simplifyVector = TRUE)
}

gfont_id <- function(family) {
  gsub("\\s+", "-", tolower(family))
}

gfont_url <- function() {
  getOption("gfonts.url", "https://google-webfonts-helper.herokuapp.com/api/fonts")
}

gfont_metadata_url <- function() {
  getOption("gfonts.metadata.url", "https://fonts.google.com/metadata/fonts")
}

gfont_format <- "ttf"
