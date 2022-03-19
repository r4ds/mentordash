#' Require Slack login to a Shiny app
#'
#' If the user hasn't logged in yet, return the login page. Otherwise return the
#' actual ui.
#'
#' @param ui A function defining the UI of the Shiny app, or a
#'   \code{\link[shiny]{tagList}}.
#' @param team A Slack team ID for which the user should be authenticated.
#'
#' @return A function defining the UI of a Shiny app (either with login or
#'   without).
#' @keywords internal
.slack_shiny_ui <- function(ui, team, redirect_uri) {
  force(ui)
  function(request) {
    if (.has_token(request)) {
      # Case 1: They already have a token. In this case we return the actual ui.

      # ui can be a tagList, a 0-argument function, or a 1-argument function.
      # Deal with that.
      if (is.list(ui)) {
        return(ui)
      } else if (is.function(ui)) {
        if (length(formals(ui))) {
          return(ui(request))
        } else {
          return(ui())
        }
      } else {
        stop("ui must be a tagList or a function.")
      }
    } else if (.has_auth_code(request)) {
      # Case 2: They are returning from the oauth endpoint, which has granted
      # them authorization. The url will now have a `code` parameter.

      # Do the call to the access url to exchange the code for a token, then
      # save that token in a cookie and redirect them to the base url of this
      # site. If they don't allow you to save a cookie, put the token in the url
      # as a parameter. 100% of this should occur in javascript. Show a GDPR
      # thing on this screen, just use the oauth_token parameter in the URL if
      # they don't accept.
      token <- slackteams::add_team_code(
        code = .extract_auth_code(request),
        redirect_uri = redirect_uri
      )
      return(
        shiny::tagList(
          shinycookie::initShinyCookie("shinycookie"),
          shiny::tags$script(
            shiny::HTML(
              sprintf(
                paste0(
                  "Cookies.set('",
                  cookie_name,
                  "', '%s', { expires: 90 });"
                ),
                token
              )
            )
          ),
          shiny::tags$script(
            shiny::HTML(
              sprintf("location.replace('%s');", redirect_uri)
            )
          )
        )
      )
    } else {
      # Case 3: They have neither a token nor a code to exchange for a token.

      # The call to this function should include an auth_url, minus the
      # redirect_uri. We should grab state if it's in the request url, but the
      # rest should come from the user of this function. We just need to add
      # redirect_uri and state and then location.replace to go there.

      # Maybe take similar arguments as httr::oauth2.0_token? Simpler, though,
      # 'cuz I'm way more opinionated. These are the things I need:

      # auth_url
      # access_url
      # client_id
      # client_secret
      # scopes
      # dots (additional named parameters)
      # maybe set_cookie and cookie_timeout
      auth_url <- slackteams::auth_url(
        scopes = slackteams::load_scopes(which = "slackverse"),
        redirect_uri = .construct_redirect_uri(request),
        team_code = team
      )
      return(
        shiny::tags$script(
          shiny::HTML(
            sprintf("location.replace(\"%s\");", auth_url)
          )
        )
      )
    }
  }
}

.has_token <- function(request) {
  .has_cookie_token(request) | .has_url_token(request)
}

.has_url_token <- function(request) {
  !is.null(.extract_url_token(request))
}

.extract_url_token <- function(request) {
  shiny::parseQueryString(request$QUERY_STRING)$oauth_token
}

.has_auth_code <- function(request) {
  # Ideally we should check for a state parameter in the query and make sure
  # it's what we expect. But we can just return the code for now.
  !is.null(.extract_auth_code(request))
}

.extract_auth_code <- function(request) {
  shiny::parseQueryString(request$QUERY_STRING)$code
}

.has_cookie_token <- function(request) {
  # This just checks for *any* cookie right now, because I need to figure out
  # how to set them before I can figure out how to parse them.
  !is.null(.extract_cookie_token(request))
}

.extract_cookie_token <- function(request) {
  .parse_cookies(request$HTTP_COOKIE)[cookie_name]
}

.parse_cookies <- function(cookies) {
  # Based on shiny::parseQueryString
  if (is.null(cookies) || nchar(cookies) == 0) {
    return(NULL)
  }
  pairs <- strsplit(cookies, "; ", fixed = TRUE)[[1]]
  pairs <- pairs[pairs != ""]
  pairs <- strsplit(pairs, "=", fixed = TRUE)
  keys <- vapply(pairs, function(x) x[1], FUN.VALUE = character(1))
  values <- vapply(pairs, function(x) x[2], FUN.VALUE = character(1))
  values[is.na(values)] <- ""
  keys <- gsub("+", " ", keys, fixed = TRUE)
  values <- gsub("+", " ", values, fixed = TRUE)
  keys <- httpuv::decodeURIComponent(keys)
  values <- httpuv::decodeURIComponent(values)
  res <- stats::setNames(as.list(values), keys)
  return(res)
}

# .construct_redirect_uri <- function(request) {
#   # Using HTTP_HOST is better but still isn't solving the problem on shinyapps.
#   host <- request$HTTP_HOST
#   redirect_uri <- paste0(
#     request$rook.url_scheme,
#     "://",
#     host
#   )
#   if (request$PATH_INFO != "/") {
#     # We want things that come before any ?, not after.
#     extra_info <- unlist(
#       strsplit(
#         x = request$PATH_INFO,
#         split = "\\?"
#       )
#     )[[1]]
#     redirect_uri <- paste0(redirect_uri, extra_info)
#   }
#   # Hack to get it to work on shinyapps for now.
#   redirect_uri <- "https://r4dscommunity.shinyapps.io/mentordash/"
#   return(redirect_uri)
# }

.team_loaded <- function(cookie_token) {
  shiny::reactive({
    slackteams::add_team_token(team_name, cookie_token)
    slackteams::activate_team(team_name)
    slackteams::get_active_team() == team_name
  })
}
