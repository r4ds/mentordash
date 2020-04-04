#' The Main App UI
#'
#' @return A \code{\link[shiny]{tagList}} containing the UI.
#' @keywords internal
.ui_main <- function() {
  dashboard_title <- "R4DS"
  shiny::tagList(
    # Leave this function for adding external resources. But commenting out
    # until I use it to avoid bugs.

    # .golem_add_external_resources(),

    # List the first level UI elements here
    shinydashboard::dashboardPage(
      header = .ui_header(dashboard_title),
      sidebar = .ui_sidebar(),
      body = .ui_body(),
      title = dashboard_title,
      skin = "blue"
    )
  )
}

#' The Dashboard Header
#'
#' @return A \code{\link[shinydashboard]{dashboardHeader}}.
#' @keywords internal
.ui_header <- function(dashboard_title) {
  shinydashboard::dashboardHeader(
    title = dashboard_title
  )
}

#' The Dashboard Sidebar
#'
#' @return A \code{\link[shinydashboard]{dashboardSidebar}}.
#' @keywords internal
.ui_sidebar <- function() {
  shinydashboard::dashboardSidebar(
    collapsed = TRUE,
    shiny::p(
      paste(
        "Eventually there will be menus here, for example to select channels",
        "to include in the report. It takes a while to load the data, sorry."
      ),
      style = "padding:4px"
    )
  )
}

#' The Dashboard Body
#'
#' @return A \code{\link[shinydashboard]{dashboardBody}}.
#' @keywords internal
.ui_body <- function() {
  shinydashboard::dashboardBody(
    shiny::h2(strong("R4DS Mentor Tool"),style = "padding-left:15px;"),
    shiny::br(),
    shiny::fluidRow(style = 'padding-left:15px;padding-right:15px;',
    shinydashboard::valueBoxOutput('valuebox_answerable'),
    shinydashboard::valueBoxOutput('valuebox_followup')
    ),
    br(),

    shinydashboard::tabBox(width = 12,id = 'questions_tabs',title = "Questions",side = 'right',
                           shiny::tabPanel(title = "Answerable Questions",
                                           value = "answerable",
                                           DT::DTOutput('answerable_questions')
                           ),
                           shiny::tabPanel(title = "Waiting for OP Followup",
                                           value = "followup",
                                           DT::DTOutput('followup_questions')
                           )
    ),
    br(),
    shiny::div(style = "text-align:center;",
               shiny::actionButton("refresh", label = "Please wait...",class = 'btn-primary')
    ),
    shiny::br(),
  )
}

#' Golem Extras
#'
#' @return A \code{\link[shiny]{tags}} head element.\
#' @keywords internal
.golem_add_external_resources <- function(){
  shiny::addResourcePath(
    'www', system.file('app/www', package = 'mentordash')
  )

  shiny::tags$head(

    # golem::activate_js(),
    # golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
