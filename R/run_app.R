#' Run the Shiny Application
#'
#' @export
run_app <- function(...) {
  golem::with_golem_options(
    app = shiny::shinyApp(ui = .ui_main, server = .app_server),
    golem_opts = list(...)
  )
}
