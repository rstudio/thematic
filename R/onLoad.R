.onLoad <- function(libname, pkgname) {
  # Leverage rsconnect's pre-deployment hook to copy the font
  # cache over to a directory that we can use on the server
  # https://github.com/rstudio/rsconnect/pull/295/files
  options(
    rsconnect.pre.deploy = function(app_dir) {
      cache_dir <- font_cache()
      font_files <- dir(cache_dir, recursive = TRUE)
      file.copy(
        file.path(cache_dir, font_files),
        file.path(app_dir, font_files)
      )
      Sys.setenv("THEMATIC_FONT_CACHE_DIR" = app_dir)
    }
  )

  if (!has_package("knitr")) return()
  if (has_package("showtext")) {
    knitr::opts_chunk$set("fig.showtext" = TRUE)
  } else if (isTRUE(getOption("knitr.in.progress"))) {
    warning(
      "The showtext package is recommended for rendering non-standard ",
      "fonts in knitr. Install it with `install.packages('showtext')`",
      call. = FALSE
    )
  }
}
