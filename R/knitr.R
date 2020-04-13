# Background color need to be set on the device level
# This ensures that, regardless of the device that knitr wants to
# use, the bg color will default to the theme's bg
knitr_dev_args_set <- function() {
  if (!isTRUE(getOption("knitr.in.progress"))) return()

  # resolve `bg = 'auto'`
  bg <- .globals$theme$bg
  if (identical(bg, "auto")) {
    bg <- auto_preferences_get()$bg %||%
      bs_theme_colors()$bg %||%
      bg
    if (identical(bg, "auto")){
      warning(
        "Couldn't detect an 'auto' bg color for the knitr dev",
        call. = FALSE
      )
      return()
    }
  }

  dev <- knitr::opts_chunk$get("dev")
  old_args <-  knitr::opts_chunk$get("dev.args")
  # Support ragg device if and when it's officially supported
  # https://github.com/yihui/knitr/pull/1834
  new_args <- rlang::set_names(
    list(bg), if (identical(dev, "ragg_png")) "background" else "bg"
  )
  if (isTRUE(dev %in% names(old_args))) {
    new_args <- rlang::set_names(list(new_args), dev)
  }

  .globals$knitr_dev_args <- old_args
  knitr::opts_chunk$set(dev.args = new_args)
}

knitr_dev_args_restore <- function() {
  if (is.null(.globals$knitr_dev_args)) return()
  knitr::opts_chunk$set(dev.args = .globals$knitr_dev_args)
  rm("knitr_dev_args", envir = .globals)
}
