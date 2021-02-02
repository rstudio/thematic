set_hooks <- function() {
  setHook("before.plot.new", base_before_hook)
  setHook("before.grid.newpage", grid_before_hook)
  setHook("plot.new", base_plot_hook)
  setHook("grid.newpage", grid_plot_hook)
}

remove_hooks <- function() {
  remove_hook("before.plot.new", base_before_hook)
  remove_hook("before.grid.newpage", grid_before_hook)
  remove_hook("plot.new", base_plot_hook)
  remove_hook("grid.newpage", grid_plot_hook)
}

remove_hook <- function(name, hook) {
  hooks <- getHook(name)
  is_thematic <- vapply(hooks, function(x) identical(x, hook), logical(1))
  setHook(name, hooks[!is_thematic], "replace")
}

base_before_hook <- function() {
  .globals$theme <- auto_resolve_theme(.globals$theme)
  # populates .globals$theme$font$family based on the first families we can support
  resolve_font_family(type = "base")
  # update graphical parameters
  base_params_set()
  base_palette_set()
}

#' @include globals.R
.globals$recordShowtext <- FALSE
base_plot_hook <- function() {
  # If showtext_begin() was called before.plot.new (this timing seems important for showtext to be effective)
  # then we record that call in plot.new hook (this timing seems important for recordGraphics to be effective).
  # Note that by recording this call, showtext works more generally in RStudio and Shiny
  # https://github.com/yixuan/showtext/issues/41#issuecomment-670983568
  if (isTRUE(.globals$recordShowtext)) {
    grDevices::recordGraphics(showtext::showtext_begin(), list(), getNamespace("showtext"))
  }
}

grid_before_hook <- function() {
  .globals$theme <- auto_resolve_theme(.globals$theme)
  # populates .globals$theme$font$family based on the first families we can support
  resolve_font_family(type = "grid")
  # update ggplot2/lattice defaults
  ggplot_build_set()
  lattice_print_set()
}

grid_plot_hook <- base_plot_hook


resolve_font_family <- function(type = c("base", "grid")) {
  font <- .globals$theme$font
  families <- font$families
  is_default_font <- identical(families, "")

  # Return early if default font family
  if (is_default_font) return()

  # Returns the name of the currently active device
  # (and, if none is active, the name of the one that *will be* used)
  dev_name <- infer_device()

  # Make sure fig.showtext = TRUE in knitr (if this is a non-ragg device)
  # (We set this .onLoad, but it only applies for the _next_ chunk)
  if (isTRUE(getOption("knitr.in.progress"))) {
    dev <- knitr::opts_current$get("dev")
    show <- knitr::opts_current$get("fig.showtext")
    if (!identical(show, TRUE) && !identical(dev, "ragg_png")) {
      stop("The fig.showtext code chunk option must be TRUE", call. = FALSE)
    }
  }

  if (is_installed("showtext") && !is_ragg_device(dev_name)) {
    showtext::showtext_begin()
    .globals$recordShowtext <- TRUE
  }

  # Since can_render() needs to open the device to detect font support,
  # we need to be able to map a device name to a function
  dev_fun <- get_device_function(dev_name)
  if (!is.function(dev_fun)) {
    stop("Internal error: get_device_function() should return a function.", call. = FALSE)
  }

  type <- match.arg(type)
  for (i in seq_along(families)) {
    family <- families[[i]]

    # Bootstrap 4 defaults to a CSS system font family...
    # we know we can't render those, so warn and try the next
    if (family %in% generic_css_families()) {
      maybe_warn(
        "Generic CSS font families (e.g. '", family, "') aren't supported. ",
        "Consider using a Google Font family instead https://fonts.google.com/",
        id = paste0("generic-css-family-", family)
      )
      next
    }

    # Register Google Fonts cache
    try_register_gfont_cache(family, systemfonts = is_ragg_device(dev_name))
    # If we can already render the font family, do no more!
    if (can_render(family, type, dev_fun, dev_name)) {
      break
    }
    # Download/register and try again
    if (font$install) {
      try_gfont_download_and_register(family, font$quiet, systemfonts = is_ragg_device(dev_name))
    }
    if (can_render(family, type, dev_fun, dev_name)) {
      break
    }

    # There's two scenarios that could cause a font to fail related to availability of a rendering device.
    pkg_install_msg <- if (!is_installed("ragg") && !is_installed("showtext")) {
      "Install ragg and/or showtext to render custom fonts. "
    } else if (is_installed("ragg") & !is_ragg_device(dev_name)) {
      "To render custom fonts, either use a ragg device or install the showtext package"
    } else {
      ""
    }

    if (nzchar(pkg_install_msg)) {
      # If we have a problem related to loaded packages rendering the font then
      # we want to just use the default font. Otherwise the plot when attempting
      # to use a non-existent font and will crash with a bunch of error messages
      # that bury our useful one above.
      family <- ""
    }

    # Try our best to give informative warning
    maybe_warn(
      "It seems the current graphics device '", dev_name, "' ",
      "is unable to render the requested font family '", family, "'. ",
      pkg_install_msg,
      id = "cant-render-font"
    )
  }

  set_font_family(family)
}

set_font_family <- function(family) {
  .globals$theme$font$family <- family
  family
}


can_render <- function(family, type = c("base", "grid"), dev_fun, dev_name) {
  # ragg devices don't produce a warning if the font family is not
  # available; instead, it uses whatever match_font(family) gives.
  # Therefore, it seems reasonable to assume the font is available
  # if the match resolves to something other than a generic font family
  if (is_ragg_device(dev_name)) {
    if (family %in% c("sans", "serif", "mono", "emoji")) return(TRUE)
    f <- systemfonts::match_font(family)
    is_available <- !identical(f, systemfonts::match_font("sans")) &&
      !identical(f, systemfonts::match_font("serif")) &&
      !identical(f, systemfonts::match_font("mono")) &&
      !identical(f, systemfonts::match_font("emoji"))
    return(is_available)
  }

  # temporarily disable thematics plot hooks
  # (otherwise, we'd get caught in an infinite loop)
  remove_hooks()
  on.exit(set_hooks(), add = TRUE)

  # To see if we have font support on the given device without
  # generating multiple pages, (temporarily) open the device
  # to render some text
  attempt_with_device(
    dev_fun = dev_fun,
    tryCatch(
      {
        if (is_installed("showtext")) showtext::showtext_begin()
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
  )
}


# Do our best to map the name of the current device to an
# actual device function
get_device_function <- function(name = infer_device()) {

  # first, resolve known cases where the .Device name
  # doesn't quite map to the relevant function name
  if ("win.metafile:" == name) {
    return(getFromNamespace("win.metafile", "grDevices"))
  }

  # Note that quartz defaults to an on-screen device,
  # so this needs to set to an off-screen type
  if ("quartz_off_screen" == name) {
    return(grDevices::png)
  }

  if ("RStudioGD" == name) {
    if (!rstudioapi::isAvailable("1.4")) {
      return(grDevices::png)
    }
    # RStudio 1.4 introduced a configurable graphics backend https://github.com/rstudio/rstudio/pull/6520
    backend <- tryNULL(readRStudioPreference("graphics_backend"))
    if ("ragg" %in% backend && is_installed("ragg")) {
      return(ragg::agg_png)
    }
    if ("windows" %in% backend && Sys.info()[["sysname"]] == "Windows") {
      return(utils::getFromNamespace("win.metafile", "grDevices"))
    }
    return(grDevices::png)
  }

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
    agg_jpeg = ragg::agg_jpeg,
    Cairo = Cairo::Cairo,
    devSVG = svglite::svglite,
    # TODO: support cairoDevices? tikz?
    stop(
      "thematic doesn't (yet) support the '", name, "' graphics device. ",
      "Please report this error to https://github.com/rstudio/thematic/issues/new",
      call. = FALSE
    )
  )
}

is_ragg_device <- function(dev_name) {
  dev_name %in% paste0("agg_", c("png", "tiff", "ppm", "jpeg"))
}



# https://drafts.csswg.org/css-fonts-4/#generic-font-families
generic_css_families <- function() {
  c(
    "serif", "sans-serif", "cursive", "fantasy", "monospace",
    "system-ui", "emoji", "math", "fangsong",
    "ui-serif", "ui-sans-serif", "ui-monospace", "ui-rounded",
    # not part of the official spec (earlier versions of system-ui)
    "-apple-system" , "BlinkMacSystemFont"
  )
}
