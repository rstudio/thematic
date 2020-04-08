# Background color need to be set on the device level
# This ensures that, regardless of the device that knitr wants to
# use, the bg color will default to the theme's bg
knitr_dev_args_set <- function() {
  if (!isTRUE(getOption("knitr.in.progress"))) return()

  dev <- knitr::opts_chunk$get("dev")
  old_args <-  knitr::opts_chunk$get("dev.args")
  new_args <- list(bg = .globals$theme$bg)
  if (isTRUE(dev %in% names(old_args))) {
    new_args <- setNames(list(new_args), dev)
  }

  .globals$knitr_dev_args <- old_args
  knitr::opts_chunk$set(dev.args = new_args)
}

knitr_dev_args_restore <- function() {
  if (is.null(.globals$knitr_dev_args)) return()
  knitr::opts_chunk$set(dev.args = .globals$knitr_dev_args)
  rm("knitr_dev_args", envir = .globals)
}
