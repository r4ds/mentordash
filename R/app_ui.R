#' The Main App UI
#'
#' @return A \code{\link[shiny]{tagList}} containing the UI.
#' @keywords internal
.app_ui <- function() {
  dashboard_title <- dashboard_name
  shinydashboard::dashboardPage(
    header = .ui_header(dashboard_title),
    sidebar = .ui_sidebar(),
    body = .ui_body(),
    title = dashboard_title,
    skin = "blue"
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
  shinydashboard::dashboardSidebar(disable = TRUE)
}

#' The Dashboard Body
#'
#' @return A \code{\link[shinydashboard]{dashboardBody}}.
#' @keywords internal
.ui_body <- function() {
  shinydashboard::dashboardBody(
    shiny::fluidRow(
      shinycookie::initShinyCookie("shinycookie")
    ),
    shiny::fluidRow(
      style = "padding-left:15px;padding-right:15px;",
      shinydashboard::valueBoxOutput("valuebox_answerable"),
      shinydashboard::valueBoxOutput("valuebox_followup")
    ),
    shiny::fluidRow(
      style = "padding-left:15px;padding-right:15px;",
      # shiny::uiOutput("question_table"),
      .question_table_output(),
      shiny::br(),
      .refresh_button_output()
    )
  )
}

.question_table_output <- function() {
  shinydashboard::tabBox(
    width = 12,
    id = "questions_tabs",
    title = "Questions",
    side = "right",
    shiny::tabPanel(
      title = "Answerable Questions",
      value = "answerable",
      DT::DTOutput("answerable_questions")
    ),
    shiny::tabPanel(
      title = "Waiting for OP Followup",
      value = "followup",
      DT::DTOutput("followup_questions")
    )
  )
}

.refresh_button_output <- function() {
  shiny::div(
    style = "text-align:center;",
    shiny::actionButton(
      "refresh",
      label = "loading, please wait...",
      class = "btn-primary"
    )
  )
}
