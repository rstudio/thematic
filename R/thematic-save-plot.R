#' Capture a thematic plot as a saved file
#'
#' Uses a `device` to capture the result of an expression (`expr`)
#' that produces a plot. If `default_device()` is used, custom fonts
#' (specified through [font_spec()]) are guaranteed to work, as long as
#' one of either the showtext or ragg package(s) are installed.
#'
#' @return the `filename` of the produced plot.
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
#' file <- thematic_save_plot(plot(1:10), res = 144)
#' if (interactive()) browseURL(file)
thematic_save_plot <- function(expr, device = default_device(),
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
      "by doing `thematic_save_plot(expr, bg_color_arg = thematic_get_option('bg'))`",
      call. = FALSE
    )
  }

  showtextOpts <- if (is.numeric(args$res) && is_installed("showtext")) {
    showtext::showtext_opts(dpi = args$res)
  }

  args[[bg_arg]] <- args[[bg_arg]] %||%
    thematic_get_option("bg", "white")

  # Resolve bg = "auto" in a similar manner to auto_resolve_theme()
  # (i.e., allow auto values to be repeatedly resolved)
  if (is_auto(args[[bg_arg]])) {
    # Should this also consider shiny::getCurrentOutputInfo()?
    args[[bg_arg]] <- auto_config_get()[["bg"]] %||%
      bs_theme_colors()[["bg"]] %||%
      args[[bg_arg]]
    if (isTRUE("auto" == args[[bg_arg]])) {
      maybe_warn(
        "Couldn't detect an 'auto' bg color to use in the graphics device.",
        id = "with-device-bg-auto"
      )
      args[[bg_arg]] <- "white"
    } else {
      args[[bg_arg]] <- parse_any_color(args[[bg_arg]])
    }
    args[[bg_arg]] <- as_auto(args[[bg_arg]])
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
  tryCatch(
    {
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
    },
    finally = {
      if (length(showtextOpts)) showtext::showtext_opts(showtextOpts)
    }
  )
}

#' Capture a plot with a graphics device
#'
#' This function is deprecated. Use [thematic_save_plot()] instead.
#'
#' @export
#' @keywords internal
thematic_with_device <- function(...) {
  .Deprecated("thematic_save_plot")
  thematic_save_plot(...)
}

#' @rdname thematic_save_plot
#' @param type the type of output format
#' @export
default_device <- function(type = c("png", "svg", "pdf", "tiff", "jpeg")) {
  type <- match.arg(type)

  if (type %in% c("png", "tiff", "jpeg") && is_installed("ragg")) {
    dev <- switch(
      type,
      png = ragg::agg_png,
      tiff = ragg::agg_tiff,
      jpeg = ragg::agg_jpeg
    )
    return(dev)
  }

  use_cairo <- is_installed("Cairo") && !capabilities("aqua")

  switch(
    type,
    png = if (use_cairo) Cairo::CairoPNG else grDevices::png,
    svg = if (use_cairo) Cairo::CairoSVG else grDevices::svg,
    pdf = if (use_cairo) Cairo::CairoPDF else grDevices::pdf,
    tiff = if (use_cairo) Cairo::CairoTIFF else grDevices::tiff,
    jpeg = if (use_cairo) Cairo::CairoJPEG else grDevices::jpeg
  )
}
