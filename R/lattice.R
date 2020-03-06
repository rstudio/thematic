lattice_params_set <- function(theme) {
  if (missing_package("lattice")) return(NULL)
  .globals$lattice_params <- lattice::trellis.par.get()
  bg <- theme$bg
  fg <- theme$fg

  lattice::trellis.par.set(
    # See figure 9.3 for an example of where grid gpar matters
    # http://lmdvr.r-forge.r-project.org/figures/figures.html
    grid.pars =         list(col = fg),
    background =        list(col = bg),
    reference.line =    list(col = bg),
    panel.background =  list(
      col = mix_colors(theme$bg, theme$fg, 0.1)
    ),
    strip.background =  list(
      col = mix_colors(theme$bg, theme$fg, 0.2)
    ),
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
    dot.line =          list(
      col = mix_colors(theme$bg, theme$fg, 0.2)
    )
  )

  # For lattice, accent can be of length 2, one to specify
  # 'stroke' accent and one for fill accent
  accent <- rep(theme$accent, length.out = 2)
  if (sum(is.na(accent)) == 0) {
    lattice::trellis.par.set(
      plot.line =         list(col = accent[[1]]),
      plot.symbol =       list(col = accent[[1]]),
      dot.symbol =        list(col = accent[[1]]),
      box.rectangle =     list(col = accent[[1]]),
      box.umbrella =      list(col = accent[[1]]),
      plot.polygon =      list(col = accent[[2]]),
      grid.pars =         list(fill = accent[[2]])
    )
  }

  qualitative <- theme$qualitative
  if (sum(is.na(qualitative)) == 0) {
    # I'm not in love with the idea of this; but alas, it's consistent with lattice's default
    region_pal <- grDevices::colorRampPalette(c(qualitative[[1]], "white", qualitative[[2]]))
    lattice::trellis.par.set(
      strip.shingle =     list(col = qualitative),
      regions           = list(col = region_pal(100)),
      superpose.line =    list(col = qualitative),
      superpose.symbol =  list(col = qualitative, fill = qualitative),
      superpose.polygon = list(col = qualitative)
    )
  }
}


lattice_params_restore <- function() {
  if (is.null(.globals$lattice_params)) return()
  lattice::trellis.par.set(theme = .globals$lattice_params)
  rm("lattice_params", envir = .globals)
}
