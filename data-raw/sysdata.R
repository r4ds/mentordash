# root_url <- "http://127.0.0.1:4242"
root_url <- "https://r4dscommunity.shinyapps.io/mentordash"

usethis::use_data(root_url, internal = TRUE, overwrite = TRUE)
rm(root_url)
