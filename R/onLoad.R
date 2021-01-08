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

  # CRAN versions
  register_upgrade_message("shiny", "1.5.0")   # shiny::getCurrentOutputInfo()
  register_upgrade_message("ggplot2", "3.3.2") # proper scale defaults
  register_upgrade_message("ragg", "0.2")      # important bug fixes

  # Dev versions
  register_upgrade_message("rmarkdown", "2.7.0", "rstudio/rmarkdown#1706")

  install_knitr_hooks()
}


# Essentially verbatim from shiny:::register_upgrade_message
register_upgrade_message <- function(pkg, version, location = NULL) {

  msg <- sprintf(
    "This version of thematic is designed to work with %s version %s or higher. ",
    pkg, version
  )

  if (length(location)) {
    msg <- paste0(msg, sprintf("Consider upgrading via remotes::install_github('%s')", location))
  }

  if (pkg %in% loadedNamespaces() && !is_available(pkg, version)) {
    packageStartupMessage(msg)
  }

  # Always register hook in case pkg is loaded at some
  # point the future (or, potentially, but less commonly,
  # unloaded & reloaded)
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      if (!is_available(pkg, version)) packageStartupMessage(msg)
    }
  )
}
is_available <- function(package, version = NULL) {
  installed <- nzchar(system.file(package = package))
  if (is.null(version)) {
    return(installed)
  }
  installed && isTRUE(utils::packageVersion(package) >= version)
}
