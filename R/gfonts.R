try_gfont_download_and_register <- function(family, quiet = TRUE) {
  # If no match in Google Font cache, look for a match with an updated set
  google_fonts <- get_google_fonts()
  family_idx <- match(family, google_fonts$family)

  if (is.na(family_idx)) {
    maybe_warn(
      "The font family '", family, "' doesn't appear to be available as a ",
      "Google Font. Try manually downloading and installing it on your system. ",
      "For more info, visit https://github.com/rstudio/thematic#fonts",
      id = paste0(family, "-not-a-google-font")
    )
    return(FALSE)
  }

  font_info <- google_fonts[family_idx, , drop = TRUE]

  download_files <- function(urls, dests) {
    urls <- urls[!is.na(urls)]
    dests <- dests[!is.na(urls)]
    Map(function(x, y) download_file(x, y, quiet = quiet), urls, dests)
  }

  cache_dir <- gfont_cache_dir(family)
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  # Columns pointin to the relevant face-specific ttf files
  # (these columns are added by add_gfont_faces())
  faces <- c("regular", "bold", "italic", "bolditalic")
  dests <- file.path(cache_dir, paste0(faces, ".ttf"))

  download_files(font_info[faces], dests)
  # Store the font info with the cache so we know if it needs
  # to be updated later on
  saveRDS(font_info, file = file.path(cache_dir, "cache.rds"))
  try_register_gfont_cache(family)
}


try_register_gfont_cache <- function(family, upgrade) {
  google_fonts <- get_google_fonts()
  family_idx <- match(family, google_fonts$family)
  if (is.na(family_idx)) return()

  cache_dir <- gfont_cache_dir(family)
  if (!dir.exists(cache_dir)) return()

  font_file <- function(variant) {
    dir(
      cache_dir, full.names = TRUE,
      pattern = paste0(variant, "\\.ttf$")
    )
  }

  regular <- font_file("regular")
  bold <- font_file("bold")
  italic <- font_file("italic")
  bolditalic <- font_file("bolditalic")

  if (is_installed("systemfonts")) {
    tryCatch(
      systemfonts::register_font(
        family, regular,
        bold = bold,
        italic = italic,
        bolditalic = bolditalic
      ),
      error = function(e) {
        msg <- conditionMessage(e)
        # It's intentional that we don't check `family %in% system_font()` before
        # registering since that's a non-trivial cost that must also be paid in
        # `register_font()`. So, if the failure is due to a system font already
        # being available, then don't throw since that's irrelevant to the end user
        # https://github.com/r-lib/systemfonts/blob/e4a98b/R/register_font.R#L60
        if (!grepl("system font with that family name", msg)) {
          warning(msg)
        }
      }
    )
  }

  if (is_installed("sysfonts")) {
    tryCatch(
      sysfonts::font_add(
        family, regular,
        bold = bold,
        italic = italic,
        bolditalic = bolditalic
      ),
      error = function(e) {
        warning(conditionMessage(e))
      }
    )
  }
}

get_google_fonts <- function() {
  .globals$google_fonts %||% google_fonts
}

update_gfonts <- function() {
  pkgs <- c("curl", "jsonlite")
  for (pkg in pkgs) {
    if (is_installed(pkg)) next
    stop("Using an updated set of google fonts requires the ", pkg, "package to be installed", call. = FALSE)
  }
  .globals$google_fonts <- tryCatch(
    add_gfont_faces(jsonlite::fromJSON(gfont_api_url())$items),
    error = function(e) {
      maybe_warn(
        "Failed to update google fonts",
        id = "update-gfonts-failed"
      )
      NULL
    }
  )
}

# Do our best to map font-weight to R's font faces
add_gfont_faces <- function(google_fonts) {
  files <- google_fonts$files
  google_fonts$regular <- files$regular %|% files$`300`

  google_fonts$bold <- files$`700` %|%
    files$`800` %|% files$`600` %|%
    files$`900` %|% files$`500`

  google_fonts$italic <- files$italic %|% files$`300italic`

  google_fonts$bolditalic <- files$`700italic` %|%
    files$`800italic` %|% files$`600italic` %|%
    files$`900italic` %|% files$`500italic`

  google_fonts
}

update_gfonts_cache <- function(quiet = FALSE) {
  font_dirs <- list.dirs(font_cache_housing(), full.names = TRUE, recursive = FALSE)
  gfonts <- get_google_fonts()
  for (font in font_dirs) {
    cache <- readRDS(file.path(font, "cache.rds"))
    lastModified <- gfonts[match(cache$family, gfonts$family), "lastModified", drop = TRUE]
    if (isTRUE(lastModified > cache$lastModified)) {
      if (!quiet) message("Updating out-of-date font cache for family '", cache$family, "'")
      try_gfont_download_and_register(cache$family, quiet)
    }
  }
}

gfont_api_url <- function() {
  paste0(
    "https://www.googleapis.com/webfonts/v1/webfonts?key=",
    gfont_key()
  )
}

gfont_key <- function() {
  Sys.getenv(
    "GFONT_KEY",
    paste0("AIzaSyDP", "KvElVqQ-", "26f7tjxyg", "IGpIajf", "tS_zmas")
  )
}
