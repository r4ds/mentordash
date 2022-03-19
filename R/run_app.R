#' Run the Shiny Application
#'
#' This function launches a Shiny App to help mentors find unanswered questions.
#'
#' @param local A logical indicating whether you are launching the app locally,
#'   rather than on shinyapps.io. Useful for debugging toward a deployed version
#'   of this app.
#'
#' @export
run_app <- function(local = FALSE, ...) {
  if (local) {
    redirect_uri <- "http://127.0.0.1:4242/"
  } else {
    redirect_uri <- redirect_uri_production
  }
  app <- shiny::shinyApp(
    ui = .slack_shiny_ui(
      .app_ui,
      team = "T6UC1DKJQ",
      redirect_uri = redirect_uri
    ),
    server = .app_server
  )
  if (local) {
    app$options$port <- 4242L
    app$options$launch.browser <- TRUE
  }

  golem::with_golem_options(
    app = app,
    golem_opts = list(...)
  )
}
