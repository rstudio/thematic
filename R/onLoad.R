.onLoad <- function(libname, pkgname) {
  # Leverage rsconnect's pre-deployment hook to copy the font
  # cache over to a directory that we can use on the server
  # https://github.com/rstudio/rsconnect/pull/295/files
  options(
    rsconnect.pre.deploy = function(app_dir) {
      font_cache_dir_set(app_dir)
    }
  )

  # Try to get the most recent set of google fonts and fallback
  # to the set shipped with the package
  .globals$google_fonts <- get_google_fonts()

  if (!rlang::is_installed("knitr")) return()
  if (rlang::is_installed("showtext")) {
    knitr::opts_chunk$set("fig.showtext" = TRUE)
  } else if (isTRUE(getOption("knitr.in.progress"))) {
    warning(
      "The showtext package is recommended for rendering non-standard ",
      "fonts in knitr. Install it with `install.packages('showtext')`",
      call. = FALSE
    )
  }
}


get_google_fonts <- function() {
  tmpfile <- tempfile(fileext = ".json")
  on.exit(unlink(tmpfile, recursive = TRUE), add = TRUE)
  tryCatch(
    {
      new_content_length <- httr::HEAD(gfont_url())$headers$`content-length`
      if (!identical(new_content_length, attr(google_fonts, "content-length"))) {
        message(
          "Google Fonts has updated since thematic was last updated.",
          "Attempting to update the set of known fonts..."
        )
        download.file(gfont_url(), tmpfile)
        jsonlite::fromJSON(tmpfile)
      }
    },
    error = function(e) google_fonts
  )
}
