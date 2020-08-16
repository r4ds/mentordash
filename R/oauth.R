#' If the user hasn't logged in yet, return the login page. Otherwise return the
#' actual ui.
#'
#' @param ui A function defining the UI of the Shiny app, or a
#'   \code{\link[shiny]{tagList}}.
#'
#' @return A function defining the UI of a Shiny app (either with login or
#'   without).
#' @keywords internal
.slack_shiny_ui <- function(ui) {
  force(ui)
  function(request) {
    if (.has_cookie_token(request)) {
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
      redirect_uri <- .construct_redirect_uri(request)
      # Change this to a promises call.
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
                "Cookies.set('r4ds_slack_token', '%s', { expires: 90 });",
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
      # I should grab state if it exists, and pass it through.
      auth_url <- slackteams::auth_url(
        redirect_uri = .construct_redirect_uri(request),
        team_code = "T6UC1DKJQ"
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
  .parse_cookies(request$HTTP_COOKIE)$r4ds_slack_token
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

.construct_redirect_uri <- function(request) {
  # if (request$SERVER_NAME == "127.0.0.1") {
  #   host <- "localhost"
  # }
  # else {
  host <- request$SERVER_NAME
  # }
  redirect_uri <- paste0(
    request$rook.url_scheme,
    "://",
    host,
    ":",
    request$SERVER_PORT
  )
  if (request$PATH_INFO != "/") {
    # We want things that come before any ?, not after.
    extra_info <- unlist(
      strsplit(
        x = request$PATH_INFO,
        split = "\\?"
      )
    )[[1]]
    redirect_uri <- paste0(redirect_uri, extra_info)
  }
  return(redirect_uri)
}

.team_loaded <- function(cookie_token) {
  shiny::reactive({
    slackteams::add_team_token("R4ds", cookie_token)
    slackteams::activate_team("R4ds")
    slackteams::get_active_team() == "R4ds"
  })
}
