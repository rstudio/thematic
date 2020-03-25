# Reusable function for registering a set of methods with S3 manually. The
# methods argument is a list of character vectors, each of which has the form
# c(package, genname, class).
registerMethods <- function(methods) {
  lapply(methods, function(method) {
    pkg <- method[[1]]
    generic <- method[[2]]
    class <- method[[3]]
    func <- get(paste(generic, class, sep="."))
    if (pkg %in% loadedNamespaces()) {
      registerS3method(generic, class, func, envir = asNamespace(pkg))
    }
    setHook(
      packageEvent(pkg, "onLoad"),
      function(...) {
        registerS3method(generic, class, func, envir = asNamespace(pkg))
      }
    )
  })
}

.onLoad <- function(libname, pkgname) {
  # Leverage rsconnect's pre-deployment hook to copy the font
  # cache over to a directory that we can use on the server
  # https://github.com/rstudio/rsconnect/pull/295/files
  options(
    rsconnect.pre.deploy = function(app_dir) {
      font_cache_set(app_dir)
    }
  )

  # Register our our build method
  registerMethods(
    list(c("ggplot2", "ggplot_build", "ggplot_thematic"))
  )

  # TODO: why must this happen onLoad in order for patchwork to work?
  ggplot_grob_set()

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
