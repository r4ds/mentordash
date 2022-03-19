cookie_name <- "r4ds_slack_token"
team_name <- "R4ds"
dashboard_name <- "R4DS Mentor Tool"
redirect_uri_production <- "https://r4dscommunity.shinyapps.io/mentordash/"

usethis::use_data(
  cookie_name,
  team_name,
  dashboard_name,
  redirect_uri_production,
  overwrite = TRUE,
  internal = TRUE
)

rm(
  cookie_name,
  team_name,
  dashboard_name,
  redirect_uri_production
)
