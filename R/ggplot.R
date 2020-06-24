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
  ggplot_build_restore()
  # Note that assignInNamespace() does S3 method registration, but to
  # find the relevant generic, it looks in the parent.frame()...
  # so this line here is prevent that from failing if ggplot2 hasn't been attached
  # https://github.com/wch/r-source/blob/d0ede8/src/library/utils/R/objects.R#L472
  ggplot_build <- getFromNamespace("ggplot_build", "ggplot2")
  .globals$ggplot_build <- getFromNamespace("ggplot_build.ggplot", "ggplot2")
  assign_in_namespace <- assignInNamespace
  assign_in_namespace("ggplot_build.ggplot", ggthematic_build, "ggplot2")
}

ggplot_build_restore <- function() {
  if (is.function(.globals$ggplot_build)) {
    ggplot_build <- getFromNamespace("ggplot_build", "ggplot2")
    assign_in_namespace <- assignInNamespace
    assign_in_namespace("ggplot_build.ggplot", .globals$ggplot_build, "ggplot2")
    rm("ggplot_build", envir = .globals)
  }
}

# N.B. If you make changes here, plotly might have to as well!
# https://github.com/ropensci/plotly/pull/1801/files#diff-3afd3a8e6a2cbc84a7afc6d2d06ec5e3R429
ggthematic_build <- function(p, ggplot_build = NULL, theme = NULL) {
  theme <- theme %||% thematic_get_theme()
  ggplot_build <- ggplot_build %||% .globals$ggplot_build
  if (!is.function(ggplot_build)) {
    stop("`ggplot_build` must be a function", call. = FALSE)
  }
  if (!length(theme)) {
    return(ggplot_build(p))
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

  # Since thematic_theme() wants to define elements that could possibly be
  # parents of the user's theme elements, we iterate through each generation of the
  # tree and merge the user theme elements with thematic_theme()
  inherits <- lapply(get_element_tree(), function(x) { x$inherit %||% "" })
  relations <- tibble::tibble(child = names(inherits), parent = as.character(inherits))
  relations <- relations[relations$parent != "", ]
  theme_cur <- theme_thematic(theme)

  p_theme <- p$theme
  while(nrow(relations) > 0) {
    this_idx <- relations$parent %in% relations$child
    this_parents <- relations$parent[!this_idx]
    this_kids <- relations$child[!this_idx]
    relations <- relations[this_idx, ]
    for (i in seq_along(this_kids)) {
      this_kid <- this_kids[[i]]
      this_parent <- this_parents[[i]]
      parent_el <- p_theme[[this_parent]]
      kid_el <- p_theme[[this_kid]]
      # parent doesn't exist don't merge at all
      if (!length(parent_el)) next
      p_theme[[this_kid]] <- if (length(kid_el)) {
        # both parent & child exist
        ggplot2::merge_element(new = kid_el, old = parent_el)
      } else {
        # parent exists but child doesnt
        parent_el
      }
    }
  }

  for (name in names(p_theme)) {
    theme_cur[[name]] <- ggplot2::merge_element(
      new = p_theme[[name]], old = theme_cur[[name]]
    )
  }

  ggplot_build(p + theme_cur)
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
