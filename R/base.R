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
    # message("set: ", bg)
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
  # message("restore: ", .globals$base_params$bg)
  do.call(graphics::par, .globals$base_params)
  rm("base_params", envir = .globals)
}
