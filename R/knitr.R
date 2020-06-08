# Background color need to be set on the device level
# This ensures that, regardless of the device that knitr wants to
# use, the bg color will default to the theme's bg
knitr_dev_args_set <- function() {
  if (!isTRUE(getOption("knitr.in.progress"))) return()

  # resolve `bg = 'auto'`
  bg <- .globals$theme$bg

  # Resolve bg = "auto" in a similar manner to auto_resolve_theme()
  # (i.e., allow auto values to be repeatedly resolved)
  if (is_auto(bg)) {
    bg <- auto_config_get()[["bg"]] %||%
      bs_theme_colors()[["bg"]] %||%
      bg
    if (isTRUE("auto" == bg)) {
      maybe_warn(
        "Couldn't detect an 'auto' bg color for the knitr dev",
        id = "knitr-device-bg-auto"
      )
      bg <- "white"
    } else {
      bg <- parse_any_color(bg)
    }
    bg <- as_auto(bg)
  }

  dev <- knitr::opts_chunk$get("dev")
  old_args <- knitr::opts_chunk$get("dev.args")
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
