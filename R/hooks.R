set_hooks <- function() {
  setHook("before.plot.new", base_before_hook)
  setHook("before.grid.newpage", grid_before_hook)
}

remove_hooks <- function() {
  remove_hook("before.plot.new", base_before_hook)
  remove_hook("before.grid.newpage", grid_before_hook)
}

remove_hook <- function(name, hook) {
  hooks <- getHook(name)
  is_thematic <- vapply(hooks, function(x) identical(x, hook), logical(1))
  setHook(name, hooks[!is_thematic], "replace")
}

base_before_hook <- function() {
  # Pick the first font family that can successfully render on this device
  font <- .globals$theme$font
  .globals$theme$font$family <- resolve_font_family(
    font$families, type = "base", font$auto_install
  )
  base_params_set()
  base_palette_set()
}

grid_before_hook <- function() {
  # Pick the first font family that can successfully render on this device
  font <- .globals$theme$font
  .globals$theme$font$family <- resolve_font_family(
    font$families, type = "grid", font$auto_install
  )
  # Updating of Geom/Scale defaults is already handled ggplot_build.ggplot_thematic
  ggplot_theme_set()
  lattice_print_set()
}


resolve_font_family <- function(families, type = c("base", "grid"), auto_install = TRUE) {
  # The default font family doesn't require special handling
  if (is_default_family(families)) {
    return(families)
  }

  # Returns the name of the currently active device
  # (and, if none is active, the name of the one that *will be* used)
  dev_name <- infer_device()

  if ("RStudioGD" %in% dev_name) {
    warning(
      call. = FALSE,
      "The RStudio's graphics device is currently unable to render non-system fonts ",
      "(i.e., thematic's Google Font support doesn't include RStudioGD). However, ",
      "if you have showtext (or ragg) is installed, this plot should render fine in shiny and rmarkdown. ",
      "To save this plot to a file (and preview), see `help(thematic_with_device)`."
    )
    return(families[1])
  }

  # Since can_render() needs to open the device to detect font support,
  # we need to be able to map a device name to a function
  dev_fun <- get_device_function(dev_name)
  if (!is.function(dev_fun)) {
    warning(
      "Thematic's Google Font support doesn't currently know about the '", dev_name, "' graphics device. ",
      "Please let us know if you see this warning: https://github.com/rstudio/thematic/issues/new",
      call. = FALSE
    )
    return(families[1])
  }

  type <- match.arg(type)
  for (i in seq_along(families)) {
    family <- families[[i]]

    # If we can already render the font family, do no more!
    try_register_gfont_cache(family)
    if (can_render(family, type, dev_fun, dev_name)) {
      break
    }

    if (auto_install) {
      if (!is_installed("showtext") && !is_installed("ragg")) {
        warning("Auto installation of fonts requires either showtext or ragg to be installed", call. = FALSE)
      } else {
        try_gfont_download_and_register(family)
      }
    }

    # Try again
    if (can_render(family, type, dev_fun, dev_name)) {
      break
    } else {
      warning(
        "It seems the current graphics device '", dev_name, "' ",
        "is unable to render the requested font family '", family, "'. ",
        "Try using `thematic_with_device()` and make sure at least one of showtext or ragg is installed.",
        call. = FALSE
      )
    }
  }
  family
}



can_render <- function(family, type = c("base", "grid"), dev_fun, dev_name) {
  # ragg devices don't produce a warning if the font family is not
  # available; instead, it uses whatever match_font(family) gives.
  # Therefore, it seems reasonable to assume the font is available
  # if the match resolves to something other than a generic font family
  if (dev_name %in% paste0("agg_", c("png", "tiff", "ppm"))) {
    if (family %in% c("sans", "serif", "mono")) return(TRUE)
    f <- systemfonts::match_font(family)
    is_available <- !identical(f, systemfonts::match_font("sans")) &&
      !identical(f, systemfonts::match_font("serif")) &&
      !identical(f, systemfonts::match_font("mono"))
    return(is_available)
  }

  # To see if we have font support on the given device without
  # generating multiple pages, (temporarily) open the device
  # to render some text
  opts <- options(device = dev_fun)
  on.exit(options(opts), add = TRUE)
  tmp <- tempfile()
  dev_new(filename = tmp)
  dev <- dev.cur()
  on.exit({
    dev.off(dev)
    unlink(tmp, recursive = TRUE)
  }, add = TRUE)

  # temporarily disable thematics plot hooks
  # (otherwise, we'd get caught in an infinite loop)
  remove_hooks()
  on.exit(set_hooks(), add = TRUE)

  # Returns TRUE if relevant plotting code runs without
  # error or warning about the font family
  tryCatch(
    {
      if (type == "grid") {
        grid.newpage()
        grid.text("testing", x = 0.5, y = 0.5, gp = gpar(fontfamily = family))
      } else {
        plot(1, family = family)
      }
      TRUE
    },
    warning = function(w) {
      !grepl(
        family,
        paste(w$message, collapse = "\n"),
        fixed = TRUE
      )
    },
    error = function(e) { FALSE }
  )
}


infer_device <- function() {
  dev_name <- names(dev.cur())
  if (!"null device" %in% dev_name) {
    return(dev_name)
  }
  # Temporarily open to a new device to infer what the device *will be*
  tmp <- tempfile()
  dev_new(filename = tmp)
  dev <- dev.cur()
  on.exit({dev.off(dev); unlink(tmp, recursive = TRUE)}, add = TRUE)
  names(dev)
}

# Do our best to map the name of the current device to an
# actual device function
get_device_function <- function(name) {
  # resolve known cases where the .Device name doesn't
  # quite map to the relevant function name
  name <- switch(
    name,
    quartz_off_screen = "quartz",
    `win.metafile:`  = "win.metafile",
    name
  )
  # Effectively what dev.new() does to find a device function from a string
  # https://github.com/wch/r-source/blob/d6c208e4/src/library/grDevices/R/device.R#L291-L293
  if (exists(name, .GlobalEnv)) {
    return(get(name, .GlobalEnv))
  }
  if (exists(name, asNamespace("grDevices"))) {
    return(get(name, asNamespace("grDevices")))
  }
  # Other important (non-grDevices) devices
  switch(
    name,
    agg_png = ragg::agg_png,
    agg_tiff = ragg::agg_tiff,
    agg_ppm = ragg::agg_ppm,
    # We don't want to suggest since it doesn't work with showtext
    Cairo = getFromNamespace("Cairo", "Cairo"),
    devSVG = getFromNamespace("svglite", "svglite"),
    # TODO: cairoDevices?
    warning("Unknown device name: '", name, "'", call. = FALSE)
  )
}


# Most devices use `filename` instead of `file`,
# but there are a few exceptions (e.g., pdf(), svglite::svglite())
dev_new <- function(filename) {
  # quartz()'s type default is "native", which is an on-screen device,
  # which can lead to suprising behavior. Fortunately, at least as far
  # as I can tell, if fonts are supported on one quartz type, it should
  # be supported for all types
  if (capabilities("aqua")) {
    opts <- grDevices::quartz.options(type = "png")
    on.exit(do.call(grDevices::quartz.options, opts), add = TRUE)
  }
  suppressMessages(dev.new(filename = filename, file = filename))
}
