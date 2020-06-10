# Logic for adjusting a color based on bg/fg/accent
adjust_color <- function(color, bg, fg, accent = NA) {
  if (!length(color)) return(color)
  if (length(color) > 1) {
    stop("Internal error: adjust_color() expects an input of length 1")
  }
  if (is.na(color) || identical(color, "NA")) return(color)

  # If a gray scale color, then the degree of gray determines
  # the mixing between fg (aka black) and bg (aka white)
  rgbs <- col2rgb(color, alpha = TRUE)[1:3,1]
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

# Estimate the amount of mixture `bg` & `fg` is takes to get `color`
# Note that method = 'cmc' gives much better results than 'cie2000'
# for the important case of (theme_gray()$panel.background):
# scales::show_col(c(mix_colors("white", "black", amount_of_mixture("gray92", "white", "black")), "gray92"))
amount_of_mixture <- function(color, bg, fg) {
  bg_dist <- farver::compare_colour(
    farver::decode_colour(color), farver::decode_colour(bg),
    from_space = "rgb", method = "cmc"
  )
  fg_dist <- farver::compare_colour(
    farver::decode_colour(color), farver::decode_colour(fg),
    from_space = "rgb", method = "cmc"
  )
  as.numeric(bg_dist / (bg_dist + fg_dist))
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

download_file <- function(url, dest, ...) {
  if (is_installed("curl")) {
    if (!curl::has_internet()) {
      warning(
        "Looks like you don't have internet access, which is needed to ",
        "download and install Google Fonts files. Try either changing ",
        "thematic::font_spec(), manually installing the relevant font, or ",
        "trying again with internet access.",
        call. = FALSE
      )
    }
    return(curl::curl_download(url, dest, ...))
  }

  if (capabilities("libcurl")) {
    return(download.file(url, dest, method = "libcurl", ...))
  }

  stop(
    "Downloading Google Font files requires either the curl package or ",
    "`capabilities('libcurl')`. ", call. = FALSE
  )
}

is_rstudio <- function(version_needed = NULL) {
  rstudioapi::isAvailable(version_needed)
}

dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

tryNULL <- function(expr) {
  tryCatch(expr, error = function(e) NULL)
}

tryGet <- function(...) {
  tryCatch(get(...), error = function(e) NULL)
}

"%||%" <- function(x, y) {
  if (!length(x)) y else x
}

"%OR%" <- function(x, y) {
  if (is.null(x) || isTRUE(is.na(x))) y else x
}
