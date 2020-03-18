
# Logic for adjusting a color based on bg/fg/accent
adjust_color <- function(color, bg, fg, accent = NA) {
  if (!length(color)) return(color)
  if (length(color) > 1) {
    warning("Failed to translated aes defaults (expected to be of length 1)")
    return(color)
  }
  if (is.na(color) || identical(color, "NA")) return(color)

  # If a gray scale color, then the degree of gray determines
  # the mixing between fg (aka black) and bg (aka white)
  rgbs <- grDevices::col2rgb(color, alpha = TRUE)[1:3,1]
  if (sum(diff(rgbs)) == 0) {
    return(mix_colors(bg, fg, 1 - (rgbs[1] / 255)))
  }

  # At this point we should be dealing with an accent color...
  # If accent is NA though, then the user has specified to NOT change it
  if (is.na(accent)) color else accent
}

mix_colors <- function(bg, fg, amount) {
  if (!length(bg) || !length(fg)) return(NULL)
  scales::colour_ramp(c(bg, fg), alpha = TRUE)(amount)
}

# x should be of length 1
parse_any_color <- function(x) {
  y <- tryCatch(
    col2rgb(x),
    error = function(e) {
      y <- htmltools::parseCssColors(x, mustWork = FALSE)
      if (is.na(y)) stop("Invalid color specification '", x, "'.", call. = FALSE)
      y
    }
  )
  if (is.character(y)) y else x
}

is_rstudio_device <- function() {
  dev <- grDevices::dev.cur()
  if (identical("RStudioGD", names(dev))) {
    return(TRUE)
  }
  if (identical("null device", names(dev)) && is_rstudio()) {
    return(TRUE)
  }
  FALSE
}

is_rstudio <- function() {
  identical("1", Sys.getenv("RSTUDIO", NA))
}


tryGet <- function(...) {
  tryCatch(get(...), error = function(e) NULL)
}

"%||%" <- function(x, y) {
  if (!length(x)) y else x
}
