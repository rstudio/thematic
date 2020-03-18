devtools::load_all()

# Download the current set of fonts
tmpfile <- tempfile(fileext = ".json")
download.file(gfont_url(), tmpfile)
google_fonts <- jsonlite::fromJSON(tmpfile)
unlink(tmpfile, recursive = TRUE)

# Save the content-length as an attribute (used for caching requests)
attr(google_fonts, "content-length") <- httr::HEAD(gfont_url())$headers$`content-length`

usethis::use_data(google_fonts, overwrite = TRUE, internal = TRUE)
