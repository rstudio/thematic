# view all shinytest differences inside package tests
# (only for internal use)
# https://github.com/rstudio/thematic/blob/main/CONTRIBUTING.md
view_shinytest_diffs <- function(path = ".", suffix = "auto", ...) {
  if (identical(suffix, "auto")) {
    branch <- system("git rev-parse --abbrev-ref HEAD", intern = TRUE)
    pieces <- strsplit(branch, "-")[[1]]
    platform <- pieces[[length(pieces)]]
    suffix <- switch(
      platform,
      Windows = "win",
      macOS = "mac",
      Linux = "linux",
      ""
    )
  }
  test_path <- file.path(path, "tests", "testthat")
  test_path <- normalizePath(test_path, mustWork = TRUE)
  test_dirs <- grep("tests/shinytest/.*-current", list.dirs(test_path), value = TRUE)
  app_dirs <- dirname(dirname(dirname(test_dirs)))
  for (dir in app_dirs) {
    getFromNamespace("viewTestDiff", "shinytest")(appDir = dir, suffix = suffix, interactive = TRUE, ...)
  }
}
