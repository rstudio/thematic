#' Control the directory used for font caching
#'
#' The default directory used for font caching is system dependent;
#' and thus, not very portable from machine to machine. Use this
#' function to move thematic's cache to a new path. This is primarily
#' useful for making font cache relative to a shiny app directory,
#' so that, when the app is deployed, the cache deploys with it.
#'
#' @param path a filepath for the new cachine directory.
#' @param cleanup whether or not to remove font files from the
#' previously used caching directory (after copying to the new location).
#'
#' @return Returns the previously used caching directory.
#' @seealso [thematic_on()], [font_spec()]
#'
#' @export
#' @examples
#' \dontrun{
#'   font_cache_set("my_app")
#'   shiny::runApp("my_app")
#' }
font_cache_set <- function(path, cleanup = FALSE) {
  old_home <- font_cache_housing()
  # Copy the existing cache over to the new cache
  font_files <- dir(old_home, recursive = TRUE)
  file.copy(
    file.path(old_home, font_files),
    file.path(path, font_files)
  )
  # Remove the old cache, if requested
  if (cleanup) {
    unlink(file.path(old_home, font_files), recursive = TRUE)
  }
  Sys.setenv("THEMATIC_FONT_CACHE_DIR" = path)
  invisible(old_home)
}

gfont_cache_dir <- function(family) {
  file.path(font_cache_housing(), gfont_id(family))
}

gfont_id <- function(family) {
  gsub("\\s+", "-", tolower(family))
}

font_cache_housing <- function() {
  Sys.getenv(
    "THEMATIC_FONT_CACHE_DIR",
    file.path(thematic_cache_dir(), "fonts")
  )
}


thematic_cache_dir <- function() {
  rappdirs::user_cache_dir("thematic")
}
