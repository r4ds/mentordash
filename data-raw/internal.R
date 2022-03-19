cookie_name <- "r4ds_slack_token"
team_name <- "R4DS Online Learning Community"
dashboard_name <- "R4DS Mentor Tool"
production_site_url <- "https://r4dscommunity.shinyapps.io/mentordash/"

usethis::use_data(
  cookie_name,
  team_name,
  dashboard_name,
  production_site_url,
  overwrite = TRUE,
  internal = TRUE
)

rm(
  cookie_name,
  team_name,
  dashboard_name,
  production_site_url
)
