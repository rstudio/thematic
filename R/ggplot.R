# ---------------------------------------------------------------
# Theme management
# ---------------------------------------------------------------

ggplot_theme_set <- function(theme = .globals$theme) {
  if (!is_installed("ggplot2")) return(NULL)
  ggplot_theme_restore()
  .globals$ggplot_theme <- update_ggtheme(theme)
}

ggplot_theme_restore <- function() {
  if (is.null(.globals$ggplot_theme)) return()
  ggplot2::theme_set(.globals$ggplot_theme)
  rm("ggplot_theme", envir = .globals)
}

update_ggtheme <- function(theme = .globals$theme) {
  # Behavior depends on the currently set theme's use of fg/bg
  # For example, theme_bw() uses fg/bg much differently than theme_gray()
  # and we should try our best to respect those design choices
  # TODO: exit early with warning if current theme is incomplete?
  old_theme <- ggplot2::theme_get()

  old <- computed_theme_elements(
    c(
      "text", "plot.background",
      "panel.background", "panel.grid",
      "legend.background", "legend.box.background", "legend.key",
      "strip.background"
    ),
    old_theme
  )

  new_fg <- theme$fg
  new_bg <- theme$bg

  # *Always* define the plot background
  ggplot2::theme_update(
    plot.background = ggplot2::element_rect(
      fill = new_bg,
      colour = "transparent"
    )
  )

  # TODO: make sure default family doesn't override a specified family
  text <- ggplot2::element_text(
    colour = new_fg,
    family = if (!identical(theme$font$family, "")) theme$font$family,
    size = old$text$size * theme$font$scale
  )
  line <- ggplot2::element_line(colour = new_fg)

  # TODO: should probably be (conditionally) updating?
  update_non_blank_elements(
    line = line,
    text = text,
    axis.title = text,
    axis.text = text,
    axis.ticks = line,
    strip.text = text
  )

  # Like %OR%, but for transparent as well
  `%missing%` <- function(x, y) {
    if (identical(x, "transparent")) return(y)
    x %OR% y
  }

  # The remaining updates depend on the old fg/bg
  old_bg <- old$plot.background$fill %missing% "white"
  old_fg <- old$text$colour %missing% "black"

  # Main idea: if a (top) color is visibly different from the color
  # underneath it (bottom), then find the amount of mixture (of fg/bg)
  # it took to obtain the top color (based on perceptual their distance),
  # and apply that amount to the new fg/bg
  update_top_color <- function(top, bottom) {
    if (identical(top %missing% bottom, bottom)) {
      return("transparent")
    }
    amt <- amount_of_mixture(top, old_bg, old_fg)
    mix_colors(new_bg, new_fg, amt)
  }

  ggplot2::theme_update(
    panel.background = ggplot2::element_rect(
      fill = update_top_color(old$panel.background$fill, old_bg)
    ),
    panel.grid = ggplot2::element_line(
      colour = update_top_color(old$panel.grid$colour, old_bg)
    ),
    legend.background = ggplot2::element_rect(
      fill = update_top_color(old$legend.background$fill, old_bg)
    ),
    legend.box.background = ggplot2::element_rect(
      fill = update_top_color(old$legend.box.background$fill, old_bg)
    ),
    legend.key = ggplot2::element_rect(
      fill = update_top_color(old$legend.key$fill, old_bg)
    ),
    # TODO: should we be comparing to panel.background?
    strip.background = ggplot2::element_rect(
      fill = update_top_color(old$strip.background$fill, old_bg)
    )
  )

  old_theme
}


computed_theme_elements <- function(x, theme) {
  setNames(lapply(x, ggplot2::calc_element, theme), x)
}

# Update only the non-blank elements in `theme`
update_non_blank_elements <- function(...) {
  is_blank <- vapply(ggplot2::theme_get(), inherits, logical(1), "element_blank")
  blank_elements <- names(is_blank)[is_blank]
  elements <- list(...)
  elements <- elements[setdiff(names(elements), blank_elements)]
  do.call(ggplot2::theme_update, elements)
}


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




ggthematic_build <- function(p, ggplot_build = .globals$ggplot_build, theme = .globals$theme) {
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
  if (!identical(qualitative, NA)) {
    scale_defaults$ggplot2.discrete.colour <- qualitative
    scale_defaults$ggplot2.discrete.fill <- qualitative
  }
  if (!identical(sequential, NA)) {
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
    if (!identical(sequential, NA)) {
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
    if (!identical(qualitative, NA)) {
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

  ggplot_build(p)
}

restore_scale <- function(name, x, envir) {
  if (is.null(x)) rm(name, envir = envir) else assign(name, x, envir = envir)
}

# Intentionally refers to a version that doesn't exist (yet).
# TODO: update version when these PRs land.
# https://github.com/tidyverse/ggplot2/pull/3828
# https://github.com/tidyverse/ggplot2/pull/3833
has_proper_ggplot_scale_defaults <- function() {
  packageVersion("ggplot2") > "4.0.0"
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
