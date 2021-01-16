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
  if (length(x) != 1) {
    stop("Internal thematic error. parse_any_color() should be used on length 1 input")
  }
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

in_rstudio_gd <- function(dev_name = infer_device()) {
  "RStudioGD" %in% dev_name
}

# If the current device is null, try to open the default device
# infer what it'll be
infer_device <- function() {
  if (!is_null_device()) {
    return(.Device)
  }
  dev <- attempt_with_new_device(.Device)
  if (!is.null(dev) && !is_null_device(dev)) {
    return(dev)
  }
  # In this case, the system's default device isn't supported,
  # but it could be that a device might be available
  # Attempt to open the default device and ask for its name
  dev <- attempt_with_device(default_device(), {.Device})
  if (!is.null(dev) && !is_null_device(dev)) {
    return(dev)
  }
  stop(
    "It seems your system doesn't support an R graphics device. ",
    "Try installing the ragg and/or Cairo packages.",
    call. = FALSE
  )
}

# .Device is equivalent to names(dev.cur())
is_null_device <- function(x = .Device) {
  identical(x, "null device")
}

attempt_with_new_device <- function(expr) {
  attempt_with_device(dev_new, expr)
}

attempt_with_device <- function(dev_fun, expr, fail_value = NULL) {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dev_before <- dev.cur()
  if (!is.function(dev_fun)) {
    stop("Internal error: dev_fun should be a function.")
  }
  file_arg <- grep("^file", names(formals(dev_fun)), value = TRUE)
  if (length(file_arg) != 1) {
    stop("Internal error: expect graphics device function to have a file/filename argument.")
  }
  res <- try(do.call(dev_fun, rlang::set_names(list(tmp), file_arg)))
  if (inherits(res, "try-error")) {
    maybe_warn(
      "thematic tried but failed to open a graphics device. If plots don't render ",
      "how you'd expect them to, try setting `options(device = ...)` to a device ",
      "that is supported on your system (e.g., `png`, `jpeg`, `Cairo::Cairo`, etc).",
      id = "no-graphics-device"
    )
    return(fail_value)
  }

  # dev.off() closes the current device, then sets the current
  # device to the _next_ device, which isn't necessarily the
  # previously open device.
  dev_after <- dev.cur()
  on.exit({
    dev.off(dev_after)
    # This next line is here to avoid this situation
    # > png(); png(); png(); dev.list()
    # quartz_off_screen quartz_off_screen quartz_off_screen
    # 2                 3                 4
    # > dev.off(); dev.cur()
    # quartz_off_screen
    # 2
    if (dev_before > 1) dev.set(dev_before)
  }, add = TRUE)

  force(expr)
}

dev_new <- function(filename) {
  # If this is called via thematic_save_plot(), then we know
  # exactly what function and args to use to clone the device
  if (length(.globals$device)) {
    do.call(.globals$device$fun, .globals$device$args)
    return()
  }
  # Most devices use `filename` instead of `file`,
  # but there are a few exceptions (e.g., pdf(), svglite::svglite())
  dev.new(filename = filename, file = filename)
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
