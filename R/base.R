base_palette_set <- function(theme = .globals$theme) {
  base_palette_restore()
  codes <- theme$qualitative
  .globals$base_palette <- if (isTRUE(is.na(codes))) {
    attempt_palette()
  } else {
    attempt_palette(codes)
  }
}

base_palette_restore <- function() {
  if (is.null(.globals$base_palette)) return()
  attempt_palette(.globals$base_palette)
  rm("base_palette", envir = .globals)
}

base_params_set <- function(theme = .globals$theme) {
  base_params_restore()
  params <- list()
  bg <- theme$bg
  if (length(bg)) {
    params <- c(params, attempt_par(bg = bg))
  }
  fg <- theme$fg
  if (length(fg)) {
    params <- c(params, attempt_par(
      fg = fg,
      col.axis = fg,
      col.lab = fg,
      col.main = fg,
      col.sub = fg
    ))
  }
  font <- theme$font
  if (length(font$family)) {
    params <- c(params, attempt_par(
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
  do.call(attempt_par, .globals$base_params)
  rm("base_params", envir = .globals)
}

attempt_par <- function(...) {
  attempt_(par(...))
}

attempt_palette <- function(...) {
  attempt_(palette(...))
}

attempt_ <- function(expr) {
  if (is_null_device()) {
    attempt_with_new_device(expr)
  } else {
    force(expr)
  }
}
