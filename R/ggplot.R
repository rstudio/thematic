# ---------------------------------------------------------------
# Theme management
# ---------------------------------------------------------------

ggplot_theme_set <- function(theme) {
  if (missing_package("ggplot2")) return(NULL)
  .globals$ggplot_theme <- ggplot2::theme_set(ggtheme_auto(theme))
}

ggplot_theme_restore <- function() {
  if (is.null(.globals$ggplot_theme)) return()
  ggplot2::theme_set(.globals$ggplot_theme)
  rm("ggplot_theme", envir = .globals)
}

ggtheme_auto <- function(theme) {
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
# Print management
#
# N.B. Overriding the print method is obviously not ideal, but compared to alternative
# approaches to setting/restoring Geom and Scale defaults, it seems like the best
# option at the moment. A future version of ggplot2 should provide sufficient tools for
# managing scale defaults management, but it's unclear whether we'll have the same
# for Geoms (this PR might do it, but it might not https://github.com/tidyverse/ggplot2/pull/2749)
# -----------------------------------------------------------------------------------

ggplot_print_set <- function(theme) {
  if (missing_package("ggplot2")) return(NULL)
  .globals$ggplot_print <- tryCatch(
    utils::getS3method("print", "ggplot"),
    error = function(e) utils::getFromNamespace("print.ggplot", "ggplot2")
  )
  registerS3method("print", "ggplot", custom_print.ggplot(theme))
}


ggplot_print_restore <- function() {
  if (is.null(.globals$ggplot_print)) return()
  registerS3method("print", "ggplot", .globals$ggplot_print)
  rm("ggplot_print", envir = .globals)
}

# N.B. this print function is designed this way because
# shiny needs the build/gtable returned from the print method
# If and when ggplot2 get proper scale/geom default mechanisms
# we can and should avoid overriding the print method
custom_print.ggplot <- function(theme = list()) {
  function(x) {
    build <- ggplot_build_with_theme(x, theme)
    gtable <- ggplot2::ggplot_gtable(build)
    grid::grid.draw(gtable)

    structure(list(
      build = build,
      gtable = gtable
    ), class = "ggplot_build_gtable")
  }
}


# This is currently needed to
ggplot_build_with_theme <- function(p, theme, ggplot_build = ggplot2::ggplot_build, newpage = TRUE) {
  if (newpage) grid::grid.newpage()
  if (!length(theme)) return(ggplot_build(p))
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
      utils::getFromNamespace, "ggplot2"
    )
  )

  # Remember defaults
  default_colours <- lapply(geoms, function(geom) geom$default_aes$colour)
  default_fills <- lapply(geoms, function(geom) geom$default_aes$fill)

  # Modify defaults
  Map(function(geom, default_color, default_fill) {
    colour <- geom$default_aes$colour
    fill <- geom$default_aes$fill
    # To avoid the possibility of modifying twice
    if (identical(colour, default_color)) {
      geom$default_aes$colour <- adjust_color(colour, bg, fg, accent)
    }
    if (identical(fill, default_fill)) {
      geom$default_aes$fill <- adjust_color(fill, bg, fg, accent)
    }
  }, geoms, default_colours, default_fills)

  # Restore defaults
  on.exit({
    Map(function(geom, colour, fill) {
      geom$default_aes$colour <- colour
      geom$default_aes$fill <- fill
    }, geoms, default_colours, default_fills)
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
  # TODO: update version depending on when these PRs are merged.
  # https://github.com/tidyverse/ggplot2/pull/3828
  # https://github.com/tidyverse/ggplot2/pull/3833
  if (has_proper_ggplot_scale_defaults()) {
    old_scales <- do.call(options, scale_defaults)
    on.exit({options(old_scales)}, add = TRUE)
  } else {
    # This isn't an officially supported way of setting default scales, but
    # `scales_add_defaults()` first looks in the plot_env to find default scales
    # https://github.com/tidyverse/ggplot2/blob/a7b3135/R/layer.r#L214
    if (!identical(sequential, NA)) {
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

has_proper_ggplot_scale_defaults <- function() {
  utils::packageVersion("ggplot2") > "3.3.0"
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
