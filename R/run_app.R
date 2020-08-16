#' Run the Shiny Application
#'
#' This function launches a Shiny App to help R4DS Online Learning Community
#' mentors find unanswered questions.
#'
#' @param local A logical indicating whether you are launching the app locally,
#'   rather than on shinyapps.io. Useful for debugging toward a deployed version
#'   of this app.
#'
#' @export
run_app <- function(local = FALSE, ...) {
  app <- shiny::shinyApp(
    ui = .slack_shiny_ui(.app_ui),
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
