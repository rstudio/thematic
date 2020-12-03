install_knitr_hooks <- function() {
  if (!is_installed("knitr")) {
    return()
  }

  # Try our best to set better chunk defaults for the fig.showtext and dev.args options...
  # Unfortunately, both of these options must be set prior to code evaluation to take effect,
  # but we won't know the "true" value that these options should be until after evaluation
  knitr::opts_chunk$set(thematic_hook = TRUE)
  knitr::opts_hooks$set(thematic_hook = function(options) {
    # Default to using showtext, but allow users to opt-out if they don't need it
    options$fig.showtext <- getOption("thematic.fig.showtext", is_installed("showtext"))
    bg <- thematic_get_option("bg", "white", resolve = TRUE)
    dev <- options[["dev"]]
    # dev.args can also be a list of lists of args
    if (is.character(dev) && isTRUE(dev %in% names(options$dev.args))) {
      options$dev.args[[dev]]$bg <- bg
    } else {
      options$dev.args$bg <- bg
    }
    options
  })
}
