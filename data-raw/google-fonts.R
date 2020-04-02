devtools::load_all()

google_fonts <- jsonlite::fromJSON(gfont_api_url())$items
google_fonts$kind <- NULL
google_fonts <- add_gfont_faces(google_fonts)
usethis::use_data(google_fonts, overwrite = TRUE, internal = TRUE)
