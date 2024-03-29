#' Run the Shiny Application
#'
#' This function launches a Shiny App to help mentors find unanswered questions.
#'
#' @param ... arguments to pass to golem_opts. See
#'   \code{\link[golem]{get_golem_options}} for more details.
#'
#' @export
run_app <- function(local = interactive(), ...) {
  app <- shinyslack::shinyslack_app(
    ui = .app_ui,
    server = .app_server,
    team_id = .team_id
  )

  golem::with_golem_options(
    app = app,
    golem_opts = list(...)
  )
}
