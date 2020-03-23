#' Control the directory used for font caching
#'
#' @return Returns the previously used caching directory.
#'
#' @param path a filepath.
#' @export
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
  Sys.setenv("THEMATIC_FONT_CACHE_DIR", path)
  invisible(old_home)
}

font_cache_family <- function(id) {
  file.path(font_cache_housing(), id)
}

font_cache_housing <- function() {
  Sys.getenv(
    "THEMATIC_FONT_CACHE_DIR",
    file.path(thematic_cache_dir(), "fonts")
  )
}

# Intentionally mimics `tools::R_user_dir("thematic", "cache")` (coming in R 4.0)
# Copyright (C) 2020 The R Core Team
# https://github.com/wch/r-source/blob/trunk/src/library/tools/R/userdir.R
thematic_cache_dir <- function() {
  home <- normalizePath("~")
  path <- if (.Platform$OS.type == "windows") {
    file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache")
  } else if (Sys.info()["sysname"] == "Darwin") {
    file.path(home, "Library", "Caches", "org.R-project.R")
  } else {
    file.path(home, ".cache")
  }
  file.path(path, "R", "thematic")
}
