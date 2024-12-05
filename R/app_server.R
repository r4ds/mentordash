#' The Main App Server Function
#'
#' @param input,output,session Shiny internal variables.
#'
#' @return Shiny reactive updates.
#' @keywords internal
.app_server <- function(input, output, session) {
  is_logged_in <- shinyslack::check_login(team_id = .team_id)
  slack_api_key <- shiny::reactive({
    shiny::req(is_logged_in())
    session$userData$shinyslack_api_key
  })
  question_channels <- shiny::reactive({
    shiny::req(is_logged_in())
    .get_question_channels(slack_api_key())
  }) |>
    shiny::bindCache(is_logged_in(), cache = "session")

  results_per_channel <- shiny::reactiveVal(10L)
  shiny::observe({
    # cli::cli_inform("Increasing results per channel.")
    results_per_channel(results_per_channel() + 10L)
  }) |>
    shiny::bindEvent(input$more, ignoreInit = TRUE)

  raw_questions <- shiny::reactive({
    shiny::req(question_channels)
    .get_raw_questions(
      question_channels(),
      results_per_channel(),
      slack_api_key()
    )
  }) |>
    shiny::bindCache(input$refresh, results_per_channel(), cache = "session") |>
    shiny::bindEvent(
      results_per_channel(),
      question_channels(),
      input$refresh
    )

  questions_df <- shiny::reactive({
    # cli::cli_inform("Refresh: {input$refresh}")
    shiny::req(raw_questions())
    .tidy_convos(raw_questions(), slack_api_key())
  }) |>
    shiny::bindCache(input$refresh, results_per_channel(), cache = "session") |>
    shiny::bindEvent(raw_questions())

  # Once the questions_df loads, update the "Refresh" button to say "Refresh".
  shiny::observe({
    shiny::updateActionButton(
      session,
      "refresh",
      label = "Refresh"
    )
    shiny::updateActionButton(
      session,
      "more",
      label = "More questions"
    )
  }) |>
    shiny::bindEvent(questions_df(), once = TRUE)

  output$valuebox_answerable <- shinydashboard::renderValueBox({
    shiny::req(questions_df())
    count_answerable <- nrow(
      dplyr::filter(questions_df(), .data$answerable)
    )

    shinydashboard::valueBox(
      count_answerable,
      subtitle = "Answerable Questions",
      icon = shiny::icon("hand-holding-heart"),
      color = "aqua"
    )
  })

  output$valuebox_followup <- shinydashboard::renderValueBox({
    shiny::req(questions_df())
    count_followup <- nrow(
      dplyr::filter(questions_df(), !.data$answerable)
    )

    shinydashboard::valueBox(
      count_followup,
      subtitle = "Waiting for OP Followup",
      icon = shiny::icon("comment-dots"),
      color = "teal"
    )
  })

  output$answerable_questions <- DT::renderDataTable({
    shiny::req(questions_df())
    questions_df() |>
      dplyr::filter(.data$answerable) |>
      dplyr::select(-"answerable", -"speech_balloon") |>
      DT::datatable(
        rownames = FALSE,
        # filter = 'top',
        selection = "single",
        escape = FALSE
      )
  })

  output$followup_questions <- DT::renderDataTable({
    questions_df() |>
      dplyr::filter(!.data$answerable) |>
      dplyr::select(-"answerable", -"speech_balloon") |>
      DT::datatable(
        rownames = FALSE,
        # filter = 'top',
        selection = "single",
        escape = FALSE
      )
  })
}

.get_question_channels <- function(slack_api_key) {
  channels <- slackteams::get_conversations_list(
    type = "public_channel",
    token = slack_api_key
  )
  question_channels_df <- dplyr::filter(
    channels,
    !.data$is_archived,
    .data$is_member,
    stringr::str_starts(.data$name, "help-")
  )

  question_channels <- rlang::set_names(
    question_channels_df$id,
    question_channels_df$name
  )
  # cli::cli_inform("Got question channels.")
  return(question_channels)
}

.get_mentors <- function(slack_api_key) {
  mentors <- slackcalls::post_slack(
    "conversations.members",
    channel = "GAJ8D7YKA",
    token = slack_api_key
  )

  if (mentors$ok && "members" %in% names(mentors)) {
    return(unlist(mentors$members))
  } else {
    # Figure out logging here so we can see who is using mentordash but isn't in
    # mentors_chat.
    return(character())
  }
}

#' Rectangle Conversation Data
#'
#' @param convos A list returned by slackthreads::conversations.
#' @param slack_api_key This user's Slack token.
#'
#' @return A tibble of question data.
#' @keywords internal
#' @importFrom rlang .data
.tidy_convos <- function(convos,
                         slack_api_key) {
  convos_tbl <- .rectangle_convos(convos)
  mentors <- .get_mentors(slack_api_key = slack_api_key)
  convos_tbl <- .simple_filter_convos_table(convos_tbl, mentors) |>
    .tag_answerable(mentors, slack_api_key) |>
    .finish_convos_table()

  return(convos_tbl)
}
