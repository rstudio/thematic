# Intentionally mimics `tools::R_user_dir("thematic", "cache")` (coming in R 4.0)
# Copyright (C) 2020 The R Core Team
# https://github.com/wch/r-source/blob/trunk/src/library/tools/R/userdir.R
cache_dir <- function() {
  home <- normalizePath("~")
  if (.Platform$OS.type == "windows") {
    file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache")
  } else if (Sys.info()["sysname"] == "Darwin") {
    file.path(home, "Library", "Caches", "org.R-project.R")
  } else {
    file.path(home, ".cache")
  }
}
