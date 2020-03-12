base_palette_set <- function(theme) {
  codes <- theme$qualitative
  .globals$base_palette <- if (isTRUE(is.na(codes))) grDevices::palette() else grDevices::palette(codes)
}

base_palette_restore <- function() {
  if (is.null(.globals$base_palette)) return()
  grDevices::palette(.globals$base_palette)
  rm("base_palette", envir = .globals)
}

base_params_set <- function(theme) {
  params <- list()
  bg <- theme$bg
  if (length(bg)) {
    params <- c(params, graphics::par(bg = bg))
  }
  fg <- theme$fg
  if (length(fg)) {
    params <- c(params, graphics::par(
      fg = fg,
      col.axis = fg,
      col.lab = fg,
      col.main = fg,
      col.sub = fg
    ))
  }
  font <- theme$font
  if (length(font$family)) {
    params <- c(params, graphics::par(
      family = font$family,
      cex.axis = font$scale,
      cex.lab = font$scale,
      cex.main = font$scale * 1.2,
      cex.sub = font$scale
    ))
  }
  .globals$base_params <- params
}

base_params_restore <- function() {
  if (is.null(.globals$base_params)) return()
  do.call(graphics::par, .globals$base_params)
  rm("base_params", envir = .globals)
}

base_params_set_hook <- function() {
  setHook("plot.new", base_params_hook)
}

base_params_restore_hook <- function() {
  plot_hooks <- getHook("plot.new")
  is_thematic_hook <- vapply(plot_hooks, function(x) identical(x, base_params_hook), logical(1))
  setHook("plot.new", plot_hooks[!is_thematic_hook], "replace")
}

base_params_hook <- structure(
  function() base_params_set(.globals$theme),
  thematic_base_params_hook = TRUE
)
