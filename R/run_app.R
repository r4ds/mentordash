#' Run the Shiny Application
#'
#' This function launches a Shiny App to help R4DS Online Learning Community
#' mentors find unanswered questions.
#'
#' @export
run_app <- function(...) {
  golem::with_golem_options(
    app = shiny::shinyApp(ui = .ui_main, server = .app_server),
    golem_opts = list(...)
  )
}
