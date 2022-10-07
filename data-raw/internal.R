.team_id <- "T6UC1DKJQ"
.site_url <- "https://r4dscommunity.shinyapps.io/mentordash/"

usethis::use_data(
  .site_url,
  .team_id,
  overwrite = TRUE,
  internal = TRUE
)

rm(
  .site_url,
  .team_id
)
