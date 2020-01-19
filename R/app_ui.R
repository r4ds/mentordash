#' The main app UI
#'
#' @return A \code{\link[shiny]{tagList}} containing the UI.
#' @keywords internal
.ui_main <- function() {
  shiny::tagList(
    # Leave this function for adding external resources
    .golem_add_external_resources(),
    # List the first level UI elements here
    shinydashboard::dashboardPage(
      header = .ui_header(),
      sidebar = .ui_sidebar(),
      body = .ui_body(),
      title = "R4DS Mentor Tool",
      skin = "blue"
    )
  )
}

.ui_header <- function() {
  shinydashboard::dashboardHeader(
    title = "R4DS Mentor Tool"
  )
}

.ui_sidebar <- function() {
  shinydashboard::dashboardSidebar(
    shiny::p(
      paste(
        "Eventually there will be menus here, for example to select channels to",
        "include in the report. It takes a while to load the data, sorry."
      ),
      style = "padding:4px"
    )
  )
}

.ui_body <- function() {
  shinydashboard::dashboardBody(
    DT::dataTableOutput("questions")
  )
}

.golem_add_external_resources <- function(){
  shiny::addResourcePath(
    'www', system.file('app/www', package = 'mentordash')
  )

  shiny::tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
