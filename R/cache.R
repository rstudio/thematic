# Intentionally mimics `tools::R_user_dir("thematic", "cache")`
# (which will be coming in R 4.0)
cache_path <- function(...) {

  final_path <- function(path, ...) {
    file.path(path, "R", "thematic", ...)
  }

  path <- Sys.getenv("R_USER_CACHE_DIR", NA)
  if (!is.na(path)) {
    return(final_path(path, ...))
  }
  path <- Sys.getenv("R_USER_CACHE_DIR", NA)
  if (!is.na(path)) {
    return(final_path(path, ...))
  }

  home <- normalizePath("~")
  path <- if (.Platform$OS.type == "windows") {
    file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache")
  } else if (Sys.info()["sysname"] == "Darwin") {
    file.path(home, "Library", "Caches", "org.R-project.R")
  } else {
    file.path(home, ".cache")
  }

  final_path(path, ...)
}
