# -----------------------------------------------------------------------------------
# Custom ggplot_build()
#
# N.B. Overriding the build method is obviously not ideal, but compared to alternative
# approaches to setting/restoring Geom and Scale defaults, it seems like the best
# option at the moment. A future version of ggplot2 should provide sufficient tools for
# managing scale defaults management, but it's unclear whether we'll have the same
# for Geoms (this PR might do it, but it might not https://github.com/tidyverse/ggplot2/pull/2749)
# -----------------------------------------------------------------------------------
ggplot_build_set <- function() {
  if (!is_installed("ggplot2")) return(NULL)
  assign_in_namespace <- assignInNamespace
  assign_in_namespace("ggplot_build", ggthematic_build, "ggplot2")
}

ggplot_build_restore <- function() {
  assign_in_namespace <- assignInNamespace
  assign_in_namespace("ggplot_build", ggplot_build_, "ggplot2")
}

#' Build a ggplot with thematic's theming
#'
#' Not intended for use by most users. It's mainly here to allow ggplot2 extension
#' packages to leverage thematic's ability to set ggplot2 defaults based on a thematic theme.
#'
#' This function does the following:
#'   1. Sets new ggplot2 defaults based on the current thematic theme.
#'   2. Calls [ggplot2::ggplot_build(p)] with the new defaults
#'   3. Restores the old defaults before exiting
#'
#' @return Returns a built ggplot using defaults informed by the current thematic theme.
#' @param p a ggplot-like object.
#' @export
ggthematic_build <- function(p) {
  UseMethod("ggthematic_build")
}

#' @export
ggthematic_build.ggplot <- function(p) {
  theme <- thematic_get_theme()
  if (!length(theme)) {
    return(ggplot_build_(p))
  }
  fg <- theme$fg
  bg <- theme$bg
  # Accent can be of length 2 because lattice
  accent <- theme$accent[1]
  qualitative <- theme$qualitative
  sequential <- theme$sequential

  # Collect all the plot's geoms, as well as some 'core' geoms,
  # since some geoms, e.g. GeomSf, want their default_aes to derive
  # from 'lower-level' geoms, like GeomPoint, GeomLine, GeomPolygon
  geoms <- c(
    lapply(p$layers, function(x) x$geom),
    lapply(
      c("GeomPoint", "GeomLine", "GeomPolygon"),
      getFromNamespace, "ggplot2"
    )
  )

  # Remember defaults
  user_defaults <- lapply(geoms, function(geom) geom$default_aes)

  # Modify defaults
  Map(function(geom, user_default) {
    geom$default_aes$colour <- adjust_color(user_default$colour, bg, fg, accent)
    geom$default_aes$fill <- adjust_color(user_default$fill, bg, fg, accent)
    # i.e., GeomText/GeomLabel
    if ("family" %in% names(geom$default_aes)) {
      geom$default_aes$family <- theme$font$family
      geom$default_aes$size <- user_default$size * theme$font$scale
    }
  }, geoms, user_defaults)

  # Restore users defaults on exit
  on.exit({
    Map(function(x, y) { x$default_aes <- y }, geoms, user_defaults)
  }, add = TRUE)

  # Modify scaling defaults
  scale_defaults <- list()
  if (!is_na_scalar(qualitative)) {
    scale_defaults$ggplot2.discrete.colour <- qualitative
    scale_defaults$ggplot2.discrete.fill <- qualitative
  }
  if (!is_na_scalar(sequential)) {
    scale_defaults$ggplot2.continuous.colour <- function(...) {
      ggplot2::scale_colour_gradientn(..., colours = sequential)
    }
    scale_defaults$ggplot2.continuous.fill <- function(...) {
      ggplot2::scale_fill_gradientn(..., colours = sequential)
    }
    scale_defaults$ggplot2.binned.colour <- function(...) {
      ggplot2::scale_colour_stepsn(..., colours = sequential)
    }
    scale_defaults$ggplot2.binned.fill <- function(...) {
      ggplot2::scale_fill_stepsn(..., colours = sequential)
    }
  }

  # If we have modern ggplot2, use the official scale default APIs.
  if (has_proper_ggplot_scale_defaults()) {
    old_scales <- do.call(options, scale_defaults)
    on.exit({options(old_scales)}, add = TRUE)
  } else {
    # This isn't an officially supported way of setting default scales, but
    # `scales_add_defaults()` first looks in the plot_env to find default scales
    # https://github.com/tidyverse/ggplot2/blob/a7b3135/R/layer.r#L214
    if (!is_na_scalar(sequential)) {
      if (!p$scales$has_scale("colour")) {
        # TODO: distinguish between existing/NULL
        colour_continuous <- tryGet("scale_colour_continuous", envir = p$plot_env)
        assign("scale_colour_continuous", scale_defaults$ggplot2.continuous.colour, envir = p$plot_env)
        on.exit({
          restore_scale("scale_colour_continuous", colour_continuous, envir = p$plot_env)
        }, add = TRUE)
      }
      if (!p$scales$has_scale("fill")) {
        fill_continuous <- tryGet("scale_fill_continuous", envir = p$plot_env)
        assign("scale_fill_continuous", scale_defaults$ggplot2.continuous.fill, envir = p$plot_env)
        on.exit({
          restore_scale("scale_fill_continuous", fill_continuous, envir = p$plot_env)
        }, add = TRUE)
      }
    }
    if (!is_na_scalar(qualitative)) {
      if (!p$scales$has_scale("colour")) {
        colour_discrete <- tryGet("scale_colour_discrete", envir = p$plot_env)
        assign(
          "scale_colour_discrete",
          function(...) ggplot2::discrete_scale("colour", "qualitative", qualitative_pal(qualitative), ...),
          envir = p$plot_env
        )
        on.exit({
          restore_scale("scale_colour_discrete", colour_discrete, envir = p$plot_env)
        }, add = TRUE)
      }

      if (!p$scales$has_scale("fill")) {
        fill_discrete <- tryGet("scale_fill_discrete", envir = p$plot_env)
        assign(
          "scale_fill_discrete",
          function(...) ggplot2::discrete_scale("fill", "qualitative", qualitative_pal(qualitative), ...),
          envir = p$plot_env
        )
        on.exit({
          restore_scale("scale_fill_discrete", fill_discrete, envir = p$plot_env)
        }, add = TRUE)
      }
    }
  }

  # Since thematic_theme() wants to elements that could possible be
  # parents of the user's theme elements, 'expand' the user's theme so
  # that theme inheritance works the way they'd expect it to
  user_theme <- lapply(names(p$theme), function(x) {
    kid_names <- find_descendants(x)
    kid_elements <- lapply(kid_names, function(y) p$theme[[y]] %||% p$theme[[x]])
    rlang::set_names(kid_elements, kid_names)
  })
  user_theme <- unlist(user_theme, recursive = FALSE)
  user_theme <- user_theme[!duplicated(names(user_theme))]
  p <- p + theme_thematic(theme) + do.call(ggplot2::theme, user_theme %||% list())

  ggplot_build_(p)
}

find_descendants <- function(parents, children = NULL) {
  tree <- ggplot2::get_element_tree()
  elements <- names(tree)
  these_kids <- elements[vapply(tree, function(x) any(parents %in% x$inherit), logical(1))]
  if (length(these_kids)) {
    find_descendants(these_kids, c(children, these_kids))
  } else {
    children
  }
}

# ----------------------------------------------------------------------
# Returns a modified version of the global theme based on thematic theme
# N.B. if and when ggplot2 gets the ability to set geom and scale defaults
# from a theme object, then it might make sense to export this function
# ----------------------------------------------------------------------
theme_thematic <- function(theme = .globals$theme) {
  # Calculate elements so we know the "final form" of each element
  # (i.e., takes care of inheritance)
  ggtheme <- computed_theme_elements(ggplot2::theme_get())

  # Handles any missing color value (e.g., NULL, NA, 'transparent')
  `%missing%` <- function(x, y) {
    if (identical(x, "transparent")) return(y)
    x %OR% y
  }

  # The remaining updates depend on the old fg/bg
  old_bg <- ggtheme$plot.background$fill %missing% .globals$base_params$bg %missing% "white"
  old_fg <- ggtheme$title$colour %missing% "black"

  new_fg <- theme$fg
  new_bg <- theme$bg

  # Main idea: theme colors are comprised of mixtures of bg and fg
  # So, given a color, get it's perceptual distance between bg <-> fg
  # and use that 'amount of mixture' to inform a new color
  update_color <- function(color) {
    amt <- amount_of_mixture(color, old_bg, old_fg)
    mix_colors(new_bg, new_fg, amt)
  }

  update_element <- function(element, name) {
    UseMethod("update_element")
  }

  update_element.element_text <- function(element, name) {
    ggplot2::element_text(
      colour = update_color(element$colour),
      size = element$size * theme$font$scale,
      family = if (!identical(theme$font$family, "")) theme$font$family
    )
  }

  update_element.element_rect <- function(element, name) {
    ggplot2::element_rect(
      fill = update_color(element$fill),
      colour = update_color(element$colour)
    )
  }

  update_element.element_line <- function(element, name) {
    ggplot2::element_line(
      colour = update_color(element$colour)
    )
  }

  update_element.element_blank <- function(element, name) {
    # Make sure plot.background is always defined; since otherwise,
    # we'd have to depend on par("bg") being set (and the device respecting it)
    if (name %in% c("plot.background", "panel.background")) {
      element <- ggplot2::element_rect(fill = new_bg, colour = new_bg)
    }
    element
  }

  update_element.default <- function(element, name) NULL

  ggtheme <- Map(function(x, y) update_element(x, y), ggtheme, names(ggtheme))
  do.call(ggplot2::theme, dropNulls(ggtheme))
}

# Get all the computed theme elements from a given theme definition
computed_theme_elements <- function(ggtheme) {
  theme_default <- ggplot2::theme_grey()
  # ggplot2 3.3.0 introduced extensible theme elements
  elements <- if (packageVersion("ggplot2") >= "3.3.0") {
    names(ggplot2::get_element_tree())
  } else {
    names(theme_default)
  }
  # If this isn't a complete theme, make it one
  # (fixes cases like ggthemes::theme_pander() which isn't complete)
  if (identical(attr(ggtheme, "complete"), FALSE)) {
    ggtheme <- theme_default + ggtheme
  }
  computed <- rlang::set_names(lapply(elements, calc_element_safe, ggtheme), elements)
  dropNulls(computed)
}

# It's not safe to calc all elements (e.g., plot.margin)
calc_element_safe <- function(x, ...) {
  tryCatch(ggplot2::calc_element(x, ...), error = function(e) x)
}

restore_scale <- function(name, x, envir) {
  if (is.null(x)) rm(name, envir = envir) else assign(name, x, envir = envir)
}

# Intentionally refers to a version that doesn't exist (yet).
# TODO: update version when these PRs land.
# https://github.com/tidyverse/ggplot2/pull/3828
# https://github.com/tidyverse/ggplot2/pull/3833
has_proper_ggplot_scale_defaults <- function() {
  packageVersion("ggplot2") >= "3.3.2"
}

qualitative_pal <- function(codes) {
  function(n) {
    if (n <= length(codes)) {
      codes[seq_len(n)]
    } else {
      scales::hue_pal()(n)
    }
  }
}

is_na_scalar <- function(x) {
  if (length(x) != 1) {
    return(FALSE)
  }
  is.na(x)
}
