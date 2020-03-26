# Apparently the only way to set lattice pars in a way that is independent
# of graphics device state is to attach those pars to the lattice object...
# This excerpt is taken from the @seealso section of help(trellis.device)
#>
#> trellis.par.get and trellis.par.set can be used to query and modify the
#> settings after a device has been initialized. The par.settings argument to
#> high level functions, described in xyplot, can be used to attach transient
#> settings to a "trellis" object.

lattice_print_set <- function(theme) {
  if (!is_installed("lattice")) return(NULL)
  lattice_print_restore()
  .globals$lattice_print <- lattice::lattice.getOption("print.function") %||%
    getFromNamespace("plot.trellis", "lattice")
  lattice::lattice.options(
    print.function = function(x, ...) {
      # Force our before.grid.newpage hook to happen *before*
      # lattice's print function is called, which needs to happen
      # in order for us to resolve the font *before* drawing
      grid::grid.newpage()
      x$par.settings <- lattice_par()
      .globals$lattice_print(x, ..., newpage = FALSE)
    }
  )
}

lattice_print_restore <- function() {
  if (!exists("lattice_print", envir = .globals)) return()
  lattice::lattice.options(print.function = .globals$lattice_print)
  rm("lattice_print", envir = .globals)
}

lattice_par <- function() {
  theme <- .globals$theme
  bg <- theme$bg
  fg <- theme$fg
  font <- theme$font

  params <- list(
    # See figure 9.3 for an example of where grid gpar matters
    # http://lmdvr.r-forge.r-project.org/figures/figures.html
    grid.pars =         list(col = fg, fontfamily = font$family, cex = font$scale),
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
    params$plot.line$col <-     accent[[1]]
    params$plot.symbol$col <-   accent[[1]]
    params$dot.symbol$col <-    accent[[1]]
    params$box.rectangle$col <- accent[[1]]
    params$box.umbrella$col <-  accent[[1]]
    params$plot.polygon$col <-  accent[[2]]
    params$grid.pars$fill <-    accent[[2]]
  }

  qualitative <- theme$qualitative
  if (sum(is.na(qualitative)) == 0) {
    # I'm not in love with the idea of this; but alas, it's consistent with lattice's default
    region_pal <- colorRampPalette(c(qualitative[[1]], bg, qualitative[[2]]))
    params$strip.shingle$col <-     qualitative
    params$regions$col <-           region_pal(100)
    params$superpose.line$col <-    qualitative
    params$superpose.symbol$col <-  qualitative
    params$superpose.symbol$fill <- qualitative
    params$superpose.polygon$col <- qualitative
  }

  params
}
