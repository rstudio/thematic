.onLoad <- function(libname, pkgname) {
  # Leverage rsconnect's pre-deployment hook to copy the font
  # cache over to a directory that we can use on the server
  # https://github.com/rstudio/rsconnect/pull/295/files
  pre_deploy <- getOption("rsconnect.pre.deploy")
  options(
    rsconnect.pre.deploy = function(app_dir) {
      if (is.function(pre_deploy)) {
        pre_deploy(app_dir)
      }
      font_cache_set(app_dir)
    }
  )

  if (!is_installed("knitr")) return()
  if (is_installed("showtext")) {
    knitr::opts_chunk$set("fig.showtext" = TRUE)
  } else if (isTRUE(getOption("knitr.in.progress"))) {
    warning(
      "The showtext package is recommended for rendering non-standard ",
      "fonts in knitr. Install it with `install.packages('showtext')`",
      call. = FALSE
    )
  }
}
