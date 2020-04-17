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

  # Suggested CRAN packages that we need recent versions of
  register_upgrade_message("systemfonts", "0.2")

  # TODO: bump these to CRAN versions when released
  register_upgrade_message("ragg", "0.1.5.9000")
  register_upgrade_message("shinytest", "1.3.1.9003")
  register_upgrade_message("htmltools", "0.4.0.9003")
  # these will take longer
  register_upgrade_message("rmarkdown", "2.2.0")
  register_upgrade_message("shiny", "1.4.0.9900")


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


# Essentially verbatim from shiny:::register_upgrade_message
register_upgrade_message <- function(pkg, version) {
  # Is an out-dated version of this package installed?
  needs_upgrade <- function() {
    if (system.file(package = pkg) == "")
      return(FALSE)
    if (utils::packageVersion(pkg) >= version)
      return(FALSE)
    TRUE
  }

  msg <- sprintf(
    "thematic is designed to work with '%s' >= %s.
    Please upgrade via install.packages('%s').",
    pkg, version, pkg
  )

  if (pkg %in% loadedNamespaces() && needs_upgrade()) {
    packageStartupMessage(msg)
  }

  # Always register hook in case pkg is loaded at some
  # point the future (or, potentially, but less commonly,
  # unloaded & reloaded)
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      if (needs_upgrade()) packageStartupMessage(msg)
    }
  )
}
