#' Capture a thematic plot as a saved file
#'
#' Uses a `device` to capture the result of an expression (`expr`)
#' that produces a plot. If `default_device()` is used, custom fonts
#' (specified through [font_spec()]) are guaranteed to work, as long as
#' one of either the showtext or ragg package(s) are installed.
#'
#' @param expr an expression that produces a plot.
#' @param device a graphics device to use for capturing the plot.
#' @param filename a filename for the produced plot. The file extension should
#' match the relevant `device`.
#' @param ... arguments passed along to the graphics `device`.
#' @export
#' @examples
#'
#' library(thematic)
#' font <- font_spec("Rock Salt", scale = 1.25)
#' thematic_on("black", "white", font = font)
#' file <- thematic_with_device(plot(1:10), res = 144)
#' if (interactive()) browseURL(file)
thematic_with_device <- function(expr, device = default_device(),
                                 filename = tempfile(fileext = ".png"), ...) {
  # N.B. implementation is quite similar to htmltools::capturePlot
  if (!is.function(device)) {
    stop(call. = FALSE, "The `device` argument should be a function, e.g. `ragg::agg_png`")
  }

  isTempFile <- missing(filename)

  # collect user and device arguments
  args <- rlang::list2(filename = filename, ...)
  device_args <- names(formals(device))

  # do our best to find the background color arg
  bg_arg <- grep("^background$|^bg$", device_args, value = TRUE)[1]
  if (isTRUE(is.na(bg_arg))) {
    stop(
      "Wasn't able to detect the background color argument for the given device, ",
      "so thematic won't automatically set it for you, but you can also set it yourself ",
      "by doing `thematic_with_device(expr, bg_color_arg = thematic_get_option('bg'))`",
      call. = FALSE
    )
  }

  args[[bg_arg]] <- args[[bg_arg]] %||%
    thematic_get_option("bg", "white")

  if (identical(args[[bg_arg]], "auto")) {
    args[[bg_arg]] <- auto_preferences_get()[["bg"]] %||%
      bs_theme_colors()[["bg"]] %||%
      rs_theme_colors()[["bg"]] %||%
      args[[bg_arg]]
    if (identical(args[[bg_arg]], "auto")){
      message(
        "Couldn't detect an 'auto' bg color to use in the graphics device.",
        call. = FALSE
      )
      args[[bg_arg]] <- "white"
    } else {
      args[[bg_arg]] <- htmltools::parseCssColors(args[[bg_arg]])
    }
  }

  # Handle the case where device wants `file` instead of `filename`
  # (e.g., svglite::svglite)
  if (!"filename" %in% device_args && "file" %in% device_args) {
    args$file <- args[["file"]] %||% args$filename
    args$filename <- NULL
  }

  # dev.off() closes the current device, then sets the current
  # device to the _next_ device, which isn't necessarily the
  # previously opened device. So, remember the current device now,
  # then open a new one, then explicitly set the device to the
  # previous device (so as to not cause side-effects).
  dev_cur <- dev.cur()
  do.call(device, args)
  on.exit({
    dev.off()
    if (dev_cur > 1) dev.set(dev_cur)
  }, add = TRUE)

  # Let the world know about the device's arguments, so that
  # resolve_font_family() can use them when cloning the device
  .globals$device <- list(fun = device, args = args)
  on.exit(rm("device", envir = .globals), add = TRUE)

  # Evaluate the expression
  expr <- rlang::enquo(expr)
  tryCatch({
    result <- withVisible(rlang::eval_tidy(expr))
    if (result$visible) {
      capture.output(print(result$value))
    }
    filename
  },
  error = function(e) {
    try({
      # i.e., if we _know_ this is a tempfile remove it before throwing
      if (isTempFile && file.exists(filename))
        unlink(filename)
    })
    stop(e)
  })
}

#' @rdname thematic_with_device
#' @param type the type of output format
#' @export
default_device <- function(type = c("png", "tiff", "svg", "pdf")) {
  type <- match.arg(type)

  if (type %in% c("png", "tiff") && is_installed("ragg")) {
    dev <- switch(
      type,
      png = ragg::agg_png,
      tiff = ragg::agg_tiff
    )
    return(dev)
  }

  if (!is_installed("showtext") && !is_default_spec(.globals$theme$font)) {
    warning(
      "Custom font rendering requires either the showtext or ragg package.",
      call. = FALSE
    )
  }

  switch(
    type,
    png = grDevices::png,
    tiff = grDevices::tiff,
    svg = grDevices::svg,
    pdf = grDevices::pdf,
    stop("'", type, "' graphics device not available.", call. = )
  )
}
