#' Run the Shiny Application
#'
#' This function launches a Shiny App to help R4DS Online Learning Community
#' mentors find unanswered questions.
#'
#' @param force_browser A logical indicating whether to force the app to launch
#'   in a browser window. Useful for debugging toward a deployed version of this
#'   app.
#'
#' @export
run_app <- function(force_browser = FALSE, ...) {
  app <- shiny::shinyApp(ui = .ui_main, server = .app_server)
  if (force_browser) {
    app$options$port <- 4242L
    app$options$launch.browser <- TRUE
  }

  golem::with_golem_options(
    app = app,
    golem_opts = list(...)
  )
}
