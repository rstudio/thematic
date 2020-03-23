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
  # Now set defaults
  knitr_dev_args_set()
  base_params_set()
  base_palette_set()
}

grid_before_hook <- function() {
  # Pick the first font family that can successfully render on this device
  font <- .globals$theme$font
  .globals$theme$font$family <- resolve_font_family(
    font$families, type = "grid", font$auto_install
  )
  # Now set defaults
  knitr_dev_args_set()
  # Updating of Geom/Scale defaults is already handled ggplot_build.ggplot_thematic
  ggplot_theme_set()
  lattice_print_set()
}


resolve_font_family <- function(families, type = c("base", "grid"), auto_install = TRUE) {
  # The default font family doesn't require special handling
  if (is_default_family(families)) {
    return(families)
  }

  type <- match.arg(type)
  for (i in seq_along(families)) {
    family <- families[[i]]

    # First register any font cache
    info <- gfont_info(family)
    if (has_gfont_cache(info)) {
      register_gfont_cache(info)
    }

    # If we can render the font family, do no more!
    if (can_render(family, type)) {
      break
    }

    if (auto_install) {
      if (!rlang::is_installed("showtext") && !rlang::is_installed("ragg")) {
        warning("Auto installation of fonts requires either showtext or ragg to be installed", call. = FALSE)
      } else {
        try_gfont_download_and_register(info, family)
      }
    }

    # Try again
    if (can_render(family, type)) {
      break
    } else {
      # TODO: check for ragg/showtext?
      warning(
        call. = FALSE,
        "It seems the current graphics device '", names(infer_device()), "' ",
        "is unable to render the requested font family '", family, "'. ",
        "Try using thematic_with_device() and make sure at least one of showtext or ragg is installed."
      )
    }
  }
  family
}



can_render <- function(family, type = c("base", "grid"), ...) {
  if (is_default_family(family)) return(TRUE)

  dev_cur <- grDevices::dev.cur()
  if (is_rstudio_device(dev_cur)) {
    warning(call. = FALSE,
      "The RStudio graphics device is unable to render auto-installed fonts. ",
      "Try using thematic_with_device() and make sure at least one of showtext or ragg is installed."
    )
    return(TRUE)
  }

  # ragg devices doesn't produce a warning if the font family is not
  # available; instead, it uses whatever match_font(family) gives.
  # Therefore, it seems reasonable to assume the font is available
  # if the match resolves to similar other than a generic font family
  if (is_ragg_device(dev_cur)) {
    if (family %in% c("sans", "serif", "mono")) return(TRUE)
    f <- systemfonts::match_font(family)
    is_available <- !identical(f, systemfonts::match_font("sans")) &&
      !identical(f, systemfonts::match_font("serif")) &&
      !identical(f, systemfonts::match_font("mono"))
    return(is_available)
  }

  # Try to open a new (temporary) device based on the current one
  # so we don't generate multiple pages
  if (!is_null_device(dev_cur)) {
    opts <- options(device = names(dev_cur))
    on.exit(options(opts), add = TRUE)
  }
  try({
    grDevices::dev.new()
    dev <- grDevices::dev.cur()
    on.exit(grDevices::dev.off(dev), add = TRUE)
  }, silent = TRUE)

  # temporarily disable thematics plot hooks
  # (otherwise, we'd get caught in an infinite loop)
  remove_hooks()
  on.exit(set_hooks(), add = TRUE)

  # Returns TRUE if relevant plotting code runs without
  # error or warning about the font family
  tryCatch(
    {
      if (type == "grid") {
        grid::grid.newpage()
        grid::grid.text("testing", x = 0.5, y = 0.5, gp = grid::gpar(fontfamily = family))
      } else {
        graphics::plot(1, family = family)
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


is_ragg_device <- function(x) {
  # TODO: support supertransparent?
  isTRUE(names(x) %in% paste0("agg_", c("png", "tiff", "ppm")))
}

is_rstudio_device <- function(x) {
  if (identical("RStudioGD", names(x))) {
    return(TRUE)
  }
  if (is_null_device(x) && is_rstudio() && interactive()) {
    return(TRUE)
  }
  FALSE
}

is_null_device <- function(x) {
  identical(names(x), "null device") && x == 1
}

infer_device <- function() {
  dev_cur <- grDevices::dev.cur()
  # If there's a device already open, then use it
  if (!is_null_device(dev_cur)) {
    return(dev_cur)
  }
  # Otherwise, temporarily open to a new device to
  # infer what the device *will be*
  grDevices::dev.new()
  dev_new <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(dev_new), add = TRUE)
  dev_new
}
