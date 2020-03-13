set_hooks <- function() {
  setHook("before.plot.new", thematic_before_hook)
  setHook("before.grid.newpage", thematic_before_hook)
  setHook("plot.new", thematic_after_hook)
  setHook("grid.newpage", thematic_after_hook)
}

restore_hooks <- function() {
  restore_hook("before.plot.new", thematic_before_hook)
  restore_hook("before.grid.newpage", thematic_before_hook)
  restore_hook("plot.new", thematic_after_hook)
  restore_hook("grid.newpage", thematic_after_hook)
}

restore_hook <- function(name, hook) {
  hooks <- getHook(name)
  is_thematic <- vapply(hooks, function(x) identical(x, hook), logical(1))
  setHook(name, hooks[!is_thematic], "replace")
}

# Every time a new page is started, we want to make sure
# the graphical parameters reflect the current thematic theme
# (opening a new graphics device modifies graphical parameters)
thematic_before_hook <- structure(
  function() {
    if (is.null(.globals$theme)) return()
    # You might wonder why we don't just do do.call(thematic_begin, theme) here...
    # Well, at least at the moment, only parameter setting seems necessary,
    # and it also seems useful to have theme_create() call plot()
    # (for detecting whether the font family is available),
    # and in that case, we get caught in an infinite loop
    base_params_set(.globals$theme)
    knitr_dev_args_set()
  },
  thematic_before_hook = TRUE
)

thematic_after_hook <- structure(
  function() {
    knitr_dev_args_restore()
  },
  thematic_after_hook = TRUE
)

# Background color need to be set on the device level
# This ensures that, regardless of the device that knitr wants to
# use, the bg color will default to the theme's bg
knitr_dev_args_set <- function() {
  if (!isTRUE(getOption("knitr.in.progress"))) return()
  # Try and accomodate any graphics device with bg/background arg,
  # but most (if not all?) currently supported knitr devices have a `bg` arg
  dev <- knitr::opts_chunk$get("dev") %||% "png"
  dev <- if (is.function(dev)) dev else match.fun(dev)
  bg_arg <- grep("^bg$|^background$", names(formals(dev)), value = TRUE)
  if (length(bg_arg)) {
    dev_args <- knitr::opts_chunk$get("dev.args")
    .globals$knitr_dev_args <- dev_args
    dev_args[[bg_arg]] <- dev_args[[bg_arg]] %||% .globals$theme$bg
    knitr::opts_chunk$set(dev.args = dev_args)
  }
}

knitr_dev_args_restore <- function() {
  if (is.null(.globals$knitr_dev_args)) return()
  knitr::opts_chunk$set(dev.args = .globals$knitr_dev_args)
  rm("knitr_dev_args", envir = .globals)
}
