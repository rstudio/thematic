base_palette_set <- function(theme = .globals$theme) {
  base_palette_restore()
  codes <- theme$qualitative
  .globals$base_palette <- if (isTRUE(is.na(codes))) {
    palette_no_new_device()
  } else {
    palette_no_new_device(codes)
  }
}

base_palette_restore <- function() {
  if (is.null(.globals$base_palette)) return()
  palette_no_new_device(.globals$base_palette)
  rm("base_palette", envir = .globals)
}

base_params_set <- function(theme = .globals$theme) {
  base_params_restore()
  params <- list()
  bg <- theme$bg
  if (length(bg)) {
    params <- c(params, par_no_new_device(bg = bg))
  }
  fg <- theme$fg
  if (length(fg)) {
    params <- c(params, par_no_new_device(
      fg = fg,
      col.axis = fg,
      col.lab = fg,
      col.main = fg,
      col.sub = fg
    ))
  }
  font <- theme$font
  if (length(font$family)) {
    params <- c(params, par_no_new_device(
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
  do.call(par_no_new_device, .globals$base_params)
  rm("base_params", envir = .globals)
}

par_no_new_device <- function(...) {
  dev <- dev.cur()
  if (dev.cur() == 1) {
    on.exit(dev.off(), add = TRUE)
  }
  par(...)
}

palette_no_new_device <- function(...) {
  dev <- dev.cur()
  if (dev.cur() == 1) {
    on.exit(dev.off(), add = TRUE)
  }
  palette(...)
}


