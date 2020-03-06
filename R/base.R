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
  .globals$base_params <- params
}

base_params_restore <- function() {
  if (is.null(.globals$base_params)) return()
  do.call(graphics::par, .globals$base_params)
  rm("base_params", envir = .globals)
}

base_palette_set <- function(theme) {
  codes <- codes_qualitative(theme)
  .globals$base_palette <- if (isTRUE(is.na(codes))) grDevices::palette() else grDevices::palette(codes)
}

base_palette_restore <- function() {
  if (is.null(.globals$base_palette)) return()
  grDevices::palette(.globals$base_palette)
  rm("base_palette", envir = .globals)
}

grid_params_set <- function(theme) {
  params <- list()
  if (length(theme$bg)) {
    params <- c(params, grid::gpar(fill = theme$bg))
  }
  if (length(theme$fg)) {
    params <- c(params, grid::gpar(col = theme$fg))
  }
  # TODO: add fontfamily when we go to support it
  .globals$grid_params <- params
}

grid_params_restore <- function() {
  if (is.null(.globals$grid_params)) return()
  do.call(grid::gpar, .globals$grid_params)
  rm("grid_params", envir = .globals)
}
