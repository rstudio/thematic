.onLoad <- function(libname, pkgname) {
  if (has_package("knitr")) {
    # Unfortunately this is too late if the package loads in the
    # same chunk as thematic_begin()...:(
    knitr::opts_knit$set("global.par" = TRUE)
  }
}
