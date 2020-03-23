# Reusable function for registering a set of methods with S3 manually. The
# methods argument is a list of character vectors, each of which has the form
# c(package, genname, class).
registerMethods <- function(methods) {
  lapply(methods, function(method) {
    pkg <- method[[1]]
    generic <- method[[2]]
    class <- method[[3]]
    func <- get(paste(generic, class, sep="."))
    if (pkg %in% loadedNamespaces()) {
      registerS3method(generic, class, func, envir = asNamespace(pkg))
    }
    setHook(
      packageEvent(pkg, "onLoad"),
      function(...) {
        registerS3method(generic, class, func, envir = asNamespace(pkg))
      }
    )
  })
}

.onLoad <- function(libname, pkgname) {
  # Leverage rsconnect's pre-deployment hook to copy the font
  # cache over to a directory that we can use on the server
  # https://github.com/rstudio/rsconnect/pull/295/files
  options(
    rsconnect.pre.deploy = function(app_dir) {
      font_cache_set(app_dir)
    }
  )

  # Register our our build method
  registerMethods(
    list(c("ggplot2", "ggplot_build", "ggplot_thematic"))
  )

  # Asynchronously update the set of google fonts (if possible)
  if (rlang::is_installed("later") &&
      rlang::is_installed("curl") &&
      rlang::is_installed("jsonlite")) {
    subscribe_fetch_content_length()
    subscribe_fetch_fonts()
    update_google_fonts()
  }

  if (!rlang::is_installed("knitr")) return()
  if (rlang::is_installed("showtext")) {
    knitr::opts_chunk$set("fig.showtext" = TRUE)
  } else if (isTRUE(getOption("knitr.in.progress"))) {
    warning(
      "The showtext package is recommended for rendering non-standard ",
      "fonts in knitr. Install it with `install.packages('showtext')`",
      call. = FALSE
    )
  }
}

subscribe_fetch_content_length <- function() {
  .globals$content_length_pool <- curl::new_pool()
  .globals$content_length_done <- FALSE
  curl::curl_fetch_multi(
    gfont_url(),
    done = function(res) {
      contentLength <- get_content_length(res)
      if (length(contentLength)) {
        .globals$content_length <- contentLength
      }
      .globals$content_length_done <- TRUE
    },
    fail = function(err) {
      .globals$content_length_done <- TRUE
    },
    pool = .globals$content_length_pool,
    # This effectively makes it a HEAD request
    # https://github.com/jeroen/curl/issues/24#issuecomment-101030581
    handle = curl::new_handle(nobody = TRUE)
  )
}

subscribe_fetch_fonts <- function() {
  .globals$fonts_pool <- curl::new_pool()
  .globals$fonts_done <- FALSE
  curl::curl_fetch_multi(
    gfont_url(),
    done = function(res) {
      body <- rawToChar(res$content)
      # Note that google_fonts() looks for this global object
      .globals$google_fonts <- structure(
        jsonlite::parse_json(body, simplifyVector = TRUE),
        "content-length" = get_content_length(res)
      )
      .globals$fonts_done <- TRUE
    },
    fail = function(res) {
      .globals$fonts_done <- TRUE
    },
    pool = .globals$fonts_pool
  )
}


update_google_fonts <- function() {
  if (!.globals$content_length_done) {
    later::later(update_google_fonts, 0.1)
    curl::multi_run(timeout = 0, pool = .globals$content_length_pool)
  } else {
    # set to TRUE to debug me
    new_fonts <- !identical(.globals$content_length, attr(google_fonts, "content-length"))
    if (new_fonts && !.globals$fonts_done) {
      later::later(update_google_fonts, 0.1)
      curl::multi_run(timeout = 0, pool = .globals$fonts_pool)
    }
  }
}

get_content_length <- function(res) {
  headers <- rawToChar(res$headers)
  m <- regexec("Content-Length:\\s+([0-9]+)", headers)
  regmatches(headers, m)[[1]][2]
}
