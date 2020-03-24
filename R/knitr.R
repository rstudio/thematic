# Background color need to be set on the device level
# This ensures that, regardless of the device that knitr wants to
# use, the bg color will default to the theme's bg
knitr_dev_args_set <- function() {
  if (!isTRUE(getOption("knitr.in.progress"))) return()
  knitr_dev_args_restore()

  .globals$knitr_dev_args <- knitr::opts_chunk$get("dev.args") %||% list()
  knitr::opts_chunk$set(
    dev.args = modifyList(
      .globals$knitr_dev_args,
      list(bg = .globals$theme$bg)
    )
  )
}

knitr_dev_args_restore <- function() {
  if (is.null(.globals$knitr_dev_args)) return()
  knitr::opts_chunk$set(dev.args = .globals$knitr_dev_args)
  rm("knitr_dev_args", envir = .globals)
}
