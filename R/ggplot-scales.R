#
# # ---------------------------------------------------------------
# # Geom defaults
# # ---------------------------------------------------------------
#
# ggplot_set_geoms <- function(theme) {
#   if (missing_package("ggplot2")) return(NULL)
#
#   bg <- theme$bg
#   fg <- theme$fg
#   # because lattice
#   accent <- theme$accent[1]
#
#   # Get all ggplot2 geoms...this terrible hack should not be necessarily
#   # after https://github.com/tidyverse/ggplot2/pull/2749
#   # TODO: in the meantime, do we provide an registry entrypoint for packages/geoms?
#   default_pkgs <- c(
#     "ggplot2"#, "ggrepel", "ggraph", "ggforce", "ggalt", "cowplot", "ggridges", "ggalluvial", "ggstance"
#   )
#   geoms <- unlist(lapply(default_pkgs, getGeomsFromPackage), recursive = FALSE)
#
#   # Remember defaults
#   default_colours <- lapply(geoms, function(geom) geom$default_aes$colour)
#   default_fills <- lapply(geoms, function(geom) geom$default_aes$fill)
#
#   # Modify defaults
#   Map(function(geom, default_color, default_fill) {
#     colour <- geom$default_aes$colour
#     fill <- geom$default_aes$fill
#     # To avoid the possibility of modifying twice
#     if (identical(colour, default_color)) {
#       geom$default_aes$colour <- adjust_color(colour, bg, fg, accent)
#     }
#     if (identical(fill, default_fill)) {
#       geom$default_aes$fill <- adjust_color(fill, bg, fg, accent)
#     }
#   }, geoms, default_colours, default_fills)
#
#   list(
#     geoms = geoms,
#     colours = default_colours,
#     fills = default_fills
#   )
# }
#
# getGeomsFromPackage <- function(pkg = "ggplot2", geoms = NULL) {
#   if (missing_package(pkg)) return(NULL)
#   if (is.null(geoms)) {
#     ggns <- asNamespace(pkg)
#     mget(ls(ggns, pattern = "^Geom[A-Z]"), ggns)
#   } else {
#     lapply(geoms, getFromNamespace, pkg)
#   }
# }
#
#
# ggplot_restore_geoms <- function() {
#   if (is.null(.globals$ggplot_geoms)) return()
#
#   geoms <- .globals$ggplot_geoms$geoms
#   colours <- .globals$ggplot_geoms$colours
#   fills <- .globals$ggplot_geoms$fills
#   Map(function(geom, colour, fill) {
#     geom$default_aes$colour <- colour
#     geom$default_aes$fill <- fill
#   }, geoms, colours, fills)
#
#   rm("ggplot_geoms", envir = .globals)
# }
#
# # ---------------------------------------------------------------
# # Scale defaults
# # ---------------------------------------------------------------
#
# ggplot_set_scales <- function(theme) {
#   if (missing_package("ggplot2")) return(NULL)
#   if (!has_proper_ggplot_scale_defaults()) return(NULL)
#
#   qualitative <- theme$qualitative
#   sequential <- theme$sequential
#
#   opts <- list()
#   if (!identical(qualitative, NA)) {
#     opts <- c(opts, options(
#       ggplot2.discrete.colour = qualitative,
#       ggplot2.discrete.fill = qualitative
#     ))
#   }
#   if (!identical(sequential, NA)) {
#     opts <- c(opts, options(
#       ggplot2.continuous.colour = function(...) {
#         ggplot2::scale_colour_gradientn(..., colours = sequential)
#       },
#       ggplot2.continuous.fill = ,
#       ggplot2.binned.colour = function(...) {
#         ggplot2::scale_colour_stepsn(..., colours = sequential)
#       },
#       ggplot2.binned.fill = function(...) {
#         ggplot2::scale_fill_stepsn(..., colours = sequential)
#       }
#     ))
#   }
#   opts
# }
#
# ggplot_restore_scales <- function() {
#   if (is.null(.globals$ggplot_scales)) return()
#
#   if (has_proper_ggplot_scale_defaults()) {
#     options(.globals$ggplot_scales)
#     return()
#   }
#
#   # TODO: what about improper scales?
#
#   rm("ggplot_scales", envir = .globals)
# }
#
# #ggplot_restore_scales <- function() {
# #  if (is.null(.globals$ggplot_scales)) return()
# #
# #  if (has_proper_ggplot_scale_defaults()) {
# #    options(.globals$ggplot_scales)
# #    return()
# #  }
# #
# #  assignInNamespace(
# #    "scale_colour_continuous", .globals$ggplot_scales$colour_continuous, "ggplot2"
# #  )
# #  assignInNamespace(
# #    "scale_fill_continuous", .globals$ggplot_scales$fill_continuous, "ggplot2"
# #  )
# #  assignInNamespace(
# #    "scale_colour_discrete", .globals$ggplot_scales$colour_discrete, "ggplot2"
# #  )
# #  assignInNamespace(
# #    "scale_fill_discrete", .globals$ggplot_scales$fill_discrete, "ggplot2"
# #  )
# #
# #  rm("ggplot_scales", envir = .globals)
# #}
