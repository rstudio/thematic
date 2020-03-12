# https://github.com/dreamRs/gfonts
google_fonts <- gfonts::get_all_fonts()
usethis::use_data(google_fonts, overwrite = TRUE, internal = TRUE)

# The above essentially does this....
#api_url <- "https://google-webfonts-helper.herokuapp.com/api/fonts"
#fonts <- curl::curl_download(api_url, "fonts.json")
#fonts <- jsonlite::fromJSON(fonts)
