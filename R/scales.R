codes_qualitative <- function(theme, n = NULL) {
  qualitative <- theme$qualitative
  if (isTRUE(is.na(qualitative)) || is.character(qualitative)) {
    return(qualitative)
  }
  # https://jfly.uni-koeln.de/color/
  okabeIto <- c("#E69F00", "#009E73", "#0072B2", "#CC79A7", "#999999", "#D55E00", "#F0E442", "#56B4E9")
  if (is.null(n)) okabeIto else okabeIto[seq_len(n)]
}

# Currently only used for ggplot2
codes_sequential <- function(theme, n = 8) {
  sequential <- theme$sequential
  if (isTRUE(is.na(sequential)) || is.character(sequential)) {
    return(
      scales::colour_ramp(sequential)(seq(0, 1, length.out = n))
    )
  }
  # Main idea: Interpolate between [fg+accent -> accent -> bg+accent]
  # For the endpoints the amount of blending of fg/bg and accent
  # depends on how similar thwt
  fg <- farver::decode_colour(theme$fg)
  accent <- farver::decode_colour(theme$accent)
  bg <- farver::decode_colour(theme$bg)
  fg_dist <- farver::compare_colour(fg, accent, from_space = "rgb", method = "cie2000")
  bg_dist <- farver::compare_colour(bg, accent, from_space = "rgb", method = "cie2000")
  fg_dist_prop <- as.numeric(fg_dist / (bg_dist + fg_dist))
  bg_dist_prop <- as.numeric(bg_dist / (bg_dist + fg_dist))
  ramp <- scales::colour_ramp(c(theme$fg, theme$accent, theme$bg), alpha = TRUE)
  ramp(
    scales::rescale(
      seq(0, 1, length.out = n),
      to = 0.5 + c(-0.5 * fg_dist_prop, 0.4 * bg_dist_prop)
    )
  )
}
