# ---------------------------------------------------------------
# Theme management
# ---------------------------------------------------------------

ggplot_theme_set <- function(theme = .globals$theme) {
  if (!is_installed("ggplot2")) return(NULL)
  ggplot_theme_restore()
  .globals$ggplot_theme <- ggplot2::theme_set(ggtheme_auto(theme))
}

ggplot_theme_restore <- function() {
  if (is.null(.globals$ggplot_theme)) return()
  ggplot2::theme_set(.globals$ggplot_theme)
  rm("ggplot_theme", envir = .globals)
}

ggtheme_auto <- function(theme = .globals$theme) {
  fg <- theme$fg
  bg <- theme$bg
  font <- theme$font

  themeGray <- ggplot2::theme_gray()

  text <- ggplot2::element_text(
    colour = fg,
    family = font$family,
    size = themeGray$text$size * font$scale
  )
  line <- ggplot2::element_line(colour = fg)

  themeGray + ggplot2::theme(
    line = line,
    text = text,
    axis.title = text,
    axis.text = text,
    axis.ticks = line,
    plot.background = ggplot2::element_rect(fill = bg, colour = "transparent"),
    panel.background = ggplot2::element_rect(
      fill = adjust_color(themeGray$panel.background$fill, bg, fg)
    ),
    panel.grid = ggplot2::element_line(colour = bg),
    legend.background = ggplot2::element_rect(fill = "transparent"),
    legend.box.background = ggplot2::element_rect(
      fill = "transparent", colour = "transparent"
    ),
    legend.key = ggplot2::element_rect(
      fill = adjust_color(themeGray$legend.key$fill, bg, fg),
      colour = bg
    ),
    strip.background = ggplot2::element_rect(
      fill = adjust_color(themeGray$strip.background$fill, bg, fg)
    ),
    strip.text = text
  )
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
  assignInNamespace("ggplot_build.ggplot", ggthematic_build, "ggplot2")
}

ggplot_build_restore <- function() {
  if (is.function(.globals$ggplot_build)) {
    ggplot_build <- getFromNamespace("ggplot_build", "ggplot2")
    assignInNamespace("ggplot_build.ggplot", .globals$ggplot_build, "ggplot2")
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
      # TODO: distinguish between existing/NULL
      colour_continuous <- tryGet("scale_colour_continuous", envir = p$plot_env)
      fill_continuous <- tryGet("scale_fill_continuous", envir = p$plot_env)
      assign("scale_colour_continuous", scale_defaults$ggplot2.continuous.colour, envir = p$plot_env)
      assign("scale_fill_continuous", scale_defaults$ggplot2.continuous.fill, envir = p$plot_env)
      on.exit({
        restore_scale("scale_colour_continuous", colour_continuous, envir = p$plot_env)
        restore_scale("scale_fill_continuous", fill_continuous, envir = p$plot_env)
      }, add = TRUE)
    }
    if (!identical(qualitative, NA)) {
      colour_discrete <- tryGet("scale_colour_discrete", envir = p$plot_env)
      fill_discrete <- tryGet("scale_fill_discrete", envir = p$plot_env)
      assign(
        "scale_colour_discrete",
        function(...) ggplot2::discrete_scale("colour", "qualitative", qualitative_pal(qualitative), ...),
        envir = p$plot_env
      )
      assign(
        "scale_fill_discrete",
        function(...) ggplot2::discrete_scale("fill", "qualitative", qualitative_pal(qualitative), ...),
        envir = p$plot_env
      )
      on.exit({
        restore_scale("scale_colour_discrete", colour_discrete, envir = p$plot_env)
        restore_scale("scale_fill_discrete", fill_discrete, envir = p$plot_env)
      }, add = TRUE)
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
