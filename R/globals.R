# TODO: .globals is currently used all over the place. Provide an official getter/setter
.globals <- new.env(parent = emptyenv())

.warning_env <- new.env(parent = emptyenv())

# Inspired by lifecycle:::needs_warning()
# https://github.com/r-lib/lifecycle/blob/ef16c98fb4/R/signal-deprecated.R#L173
needs_warning <- function(id) {
  last <- .warning_env[[id]]
  if (is.null(last)) {
    return(TRUE)
  }

  if (!inherits(last, "POSIXct")) {
    stop("Internal error: Expected `POSIXct` value in `needs_warning()`.", call. = FALSE)
  }

  # Warn every 8 hours
  (Sys.time() - last) > (8 * 60 * 60)
}


maybe_warn <- function(..., id, call. = FALSE) {
  if (needs_warning(id)) {
    .warning_env[[id]] <- Sys.time()
    warning(..., call. = call.)
  }
}
