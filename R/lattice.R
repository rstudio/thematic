# Apparently the only way to set lattice pars in a way that is independent
# of graphics device state is to attach those pars to the lattice object...
# This excerpt is taken from the @seealso section of help(trellis.device)
#>
#> trellis.par.get and trellis.par.set can be used to query and modify the
#> settings after a device has been initialized. The par.settings argument to
#> high level functions, described in xyplot, can be used to attach transient
#> settings to a "trellis" object.

lattice_print_set <- function(theme) {
  if (missing_package("lattice")) return(NULL)
  print_function <- lattice::lattice.getOption("print.function")
  .globals$lattice_print <- print_function
  lattice::lattice.options(
    print.function = function(x, ...) {
      x$par.settings <- lattice_par(theme)
      print_function <- print_function %||% utils::getFromNamespace("plot.trellis", "lattice")
      print_function(x, ...)
    }
  )
}

lattice_print_restore <- function() {
  if (!exists("lattice_print", envir = .globals)) return()
  lattice::lattice.options(print.function = .globals$lattice_print)
  rm("lattice_print", envir = .globals)
}

lattice_par <- function(theme) {
  bg <- theme$bg
  fg <- theme$fg
  font <- theme$font

  params <- list(
    # See figure 9.3 for an example of where grid gpar matters
    # http://lmdvr.r-forge.r-project.org/figures/figures.html
    grid.pars = list(
      # TODO: does font family need to go anywhere else?
      fontfamily = font$family, col = fg, cex = font$scale
    ),
    background =        list(col = bg),
    reference.line =    list(col = bg),
    panel.background =  list(col = mix_colors(theme$bg, theme$fg, 0.1)),
    strip.background =  list(col = mix_colors(theme$bg, theme$fg, 0.2)),
    strip.border =      list(col = fg),
    axis.line =         list(col = fg),
    axis.text =         list(col = fg),
    add.line =          list(col = fg),
    add.text =          list(col = fg),
    par.xlab.text =     list(col = fg),
    par.ylab.text =     list(col = fg),
    par.zlab.text =     list(col = fg),
    par.main.text =     list(col = fg),
    par.sub.text =      list(col = fg),
    box.3d =            list(col = fg),
    plot.polygon =      list(border = fg),
    superpose.polygon = list(border = fg),
    box.dot =           list(col = fg),
    dot.line =          list(col = mix_colors(theme$bg, theme$fg, 0.2))
  )

  # For lattice, accent can be of length 2, one to specify
  # 'stroke' accent and one for fill accent
  accent <- rep(theme$accent, length.out = 2)
  if (sum(is.na(accent)) == 0) {
    params$plot.line <-     list(col = accent[[1]])
    params$plot.symbol <-   list(col = accent[[1]])
    params$dot.symbol <-    list(col = accent[[1]])
    params$box.rectangle <- list(col = accent[[1]])
    params$box.umbrella <-  list(col = accent[[1]])
    params$plot.polygon <-  list(col = accent[[2]])
    params$grid.pars <-     list(fill = accent[[2]])
  }

  qualitative <- theme$qualitative
  if (sum(is.na(qualitative)) == 0) {
    # I'm not in love with the idea of this; but alas, it's consistent with lattice's default
    region_pal <- grDevices::colorRampPalette(c(qualitative[[1]], "white", qualitative[[2]]))
    params$strip.shingle <-     list(col = qualitative)
    params$regions <-           list(col = region_pal(100))
    params$superpose.line <-    list(col = qualitative)
    params$superpose.symbol <-  list(col = qualitative, fill = qualitative)
    params$superpose.polygon <- list(col = qualitative)
  }

  params
}
