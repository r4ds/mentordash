#' The Main App Server Function
#'
#' @param input,output,session Shiny internal variables.
#'
#' @return Shiny reactive updates.
#' @keywords internal
.app_server <- function(input, output, session) {
  # If they're here, they're authenticated with Slack, and the proper
  # environment variable is set for shinyslack to use.
  question_channels <- shiny::reactive(.get_question_channels())

  questions_df <- shiny::eventReactive(
    input$refresh,
    {
      shiny::req(question_channels())
      .get_questions(question_channels())
    },
    ignoreNULL = FALSE
  )

  # Once the questions_df loads, update the "Refresh" button to say "Refresh".
  shiny::observeEvent(
    questions_df(),
    shiny::updateActionButton(
      session,
      "refresh",
      label = "Refresh"
    ),
    once = TRUE
  )

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

  # output$question_table <- shiny::renderUI({
  #   shiny::req(questions_df())
  #   .question_table_output()
  # })

  output$answerable_questions <- DT::renderDataTable({
    shiny::req(questions_df())
    questions_df() %>%
      dplyr::filter(.data$answerable) %>%
      dplyr::select(-"answerable", -"speech_balloon") %>%
      DT::datatable(
        rownames = FALSE,
        # filter = 'top',
        selection = "single",
        escape = FALSE
      )
  })

  output$followup_questions <- DT::renderDataTable({
    questions_df() %>%
      dplyr::filter(!.data$answerable) %>%
      dplyr::select(-"answerable", -"speech_balloon") %>%
      DT::datatable(
        rownames = FALSE,
        # filter = 'top',
        selection = "single",
        escape = FALSE
      )
  })
}

.get_question_channels <- function() {
  channels <- slackteams:::get_conversations_list(type = "public_channel")
  question_channels_df <- dplyr::filter(
    channels, stringr::str_starts(.data$name, "help-")
  )

  question_channels <- rlang::set_names(
    question_channels_df$id,
    question_channels_df$name
  )
  return(question_channels)
}

#' Get Question Threads
#'
#' @return A tidy tibble of question information.
#' @keywords internal
.get_questions <- function(question_channels) {
  ## Read in Conversations ----
  total_results <- 100L

  convos <- purrr::map(
    question_channels,
    slackthreads::conversations,
    max_results = total_results,
    limit = min(total_results, 1000L)
  )

  ## Response to Tibble ----
  convos_tbl <- .tidy_convos(convos)

  return(convos_tbl)
}

#' Rectangle Conversation Data
#'
#' @param convos A list returned by slackthreads::conversations.
#' @param question_channels A named character vector, where the name is the
#'   user-friendly name of the channel, and the internal character is the
#'   channel ID.
#'
#' @return A tibble of question data.
#' @keywords internal
#' @importFrom rlang .data
.tidy_convos <- function(convos) {
  # For now I'm filtering stuff out here that's "done". Later we'll make these
  # explicit filters.

  bad_subtypes <- c("channel_join", "channel_name", "bot_add", "bot_message")
  keep_timeframe <- lubridate::weeks(3)

  convos_tbl <- purrr::map_dfr(
    convos,
    function(this_channel) {
      # Drop the special class so tibble doesn't freak out.
      class(this_channel) <- "list"
      tibble::tibble(
        channel_id = attr(this_channel, "channel"),
        conversations = as.list(this_channel)
      )
    },
    .id = "channel"
  ) %>%
    # A general unnest_wider was breaking, so let's just get the channels we
    # care about.
    tidyr::hoist(
      .data$conversations,
      "ts",
      "user",
      "subtype",
      "latest_reply",
      "reactions",
      "reply_count",
      "reply_users",
      "thread_ts",
      "text"
    ) %>%
    dplyr::mutate(
      reply_count = tidyr::replace_na(.data$reply_count, 0)
    )

  convos_tbl <- convos_tbl %>%
    # Get rid of channel_join and channel_name.
    dplyr::filter(
      !(.data$subtype %in% bad_subtypes),
      .data$user != "USLACKBOT"
    ) %>%
    # Only keep "recent" threads.
    dplyr::mutate(
      latest_activity = as.POSIXct(
        purrr::map2_dbl(
          as.numeric(.data$ts), as.numeric(.data$latest_reply),
          max,
          na.rm = TRUE
        ),
        origin = "1970-01-01"
      )
    ) %>%
    dplyr::filter(
      latest_activity >= (lubridate::now() - keep_timeframe)
    ) %>%
    dplyr::mutate(
      heavy_check_mark = .has_reaction(
        .data$reactions,
        c("heavy_check_mark", "question-answered", "white_check_mark")
      ),
      thread_tag = .has_reaction(
        .data$reactions,
        "thread"
      ),
      nevermind = .has_reaction(
        .data$reactions,
        c("question-nevermind", "octagonal_sign", "nevermind")
      )
    ) %>%
    dplyr::filter(
      !.data$heavy_check_mark,
      !.data$thread_tag,
      !.data$nevermind
    ) %>%
    dplyr::mutate(
      speech_balloon = .has_reaction(
        .data$reactions,
        c("speech_balloon", "question-more-info")
      ),
      answerable = .is_answerable(
        .data$speech_balloon, .data$user, .data$reply_users,
        channel_ids = .data$channel_id, tses = .data$thread_ts
      )
    ) %>%
    # Seeing if dumping "waiting for op followup" here speeds things up.
    # dplyr::filter(answerable) %>%
    dplyr::mutate(
      `web link` = purrr::map2(
        .data$channel_id, .data$ts,
        function(chnl, this_ts) {
          paste0(
            "<a href=\"",
            paste(
              "https://app.slack.com/client/T6UC1DKJQ",
              chnl,
              this_ts,
              sep = "/"
            ),
            "\", target=\"_blank\">Web</a>"
          )
        }
      ),
      # `app link` = purrr::map2(
      #   .data$channel_id, .data$ts,
      #   function(chnl, this_ts) {
      #     paste0(
      #       "<a href=\"",
      #       paste(
      #         "https://rfordatascience.slack.com/archives",
      #         chnl,
      #         paste0("p", sub(x = this_ts, "\\.", "")),
      #         sep = "/"
      #       ),
      #       "\", target=\"_blank\">App</a>"
      #     )
      #   }
      # ),
      excerpt = stringr::str_trunc(text, 100),
      # links = paste(`web link`, `app link`, sep = " | "),
      latest_activity = format(latest_activity, "%Y-%m-%d %H:%M:%S")
    ) %>%
    dplyr::select(
      .data$channel,
      .data$excerpt,
      .data$reply_count,
      .data$speech_balloon,
      .data$answerable,
      .data$`web link`,
      # .data$links,
      .data$latest_activity
    ) %>%
    dplyr::arrange(.data$latest_activity)

  return(convos_tbl)
}

#' Check for a Reaction in Reactions
#'
#' @param reactions A list of reaction lists.
#' @param reaction_name A character vector with the name of one or more target
#'   reactions.
#'
#' @return A logical vector the same length as reactions.
#' @keywords internal
.has_reaction <- function(reactions, reaction_name) {
  purrr::map_lgl(reactions, function(rxns) {
    if (all(is.na(rxns))) {
      FALSE
    } else {
      any(
        purrr::map_lgl(
          rxns,
          function(rxn) {
            rxn$name %in% reaction_name
          }
        )
      )
    }
  })
}

#' Check Whether a Question is Answerable
#'
#' @param speech_balloons The logical vector indicating whether a question is
#'   tagged with the "needs more information" emoji.
#' @param users The character vector of users who posted the question.
#' @param reply_userses The list of character vectors of users who have replied
#'   to this thread.
#' @param channels The character vector of channels in which the questions were
#'   posted.
#' @param tses The character vector of timestamps for threads attached to the
#'   question.
#'
#' @return A logical vector.
#' @keywords internal
.is_answerable <- function(speech_balloons,
                           users,
                           reply_userses,
                           channel_ids,
                           tses) {
  # We need to return a logical vector indicating whether this question is
  # "answerable", meaning the thread hasn't been tagged as needing more info OR
  # there isn't a reply OR the latest reply was by the user. To determine the
  # latest reply, we need to do an extra slack call, so try to do that as little
  # as possible. And try to vectorize this cleanly.

  answerable <- !speech_balloons | is.na(tses)

  # Test the FALSEs.

  # If there aren't any reply_users, it's answerable. This... shouldn't ever
  # actually get triggered.
  answerable[!answerable] <- purrr::map_lgl(
    reply_userses[!answerable],
    ~ !as.logical(length(.x))
  )

  # We need to check if the op was the *most recent* reply. This can be slow.
  answerable[!answerable] <- purrr::pmap_lgl(
    .l = list(
      ts = tses[!answerable],
      channel_id = channel_ids[!answerable],
      user = users[!answerable],
      reply_users = reply_userses[!answerable]
    ),
    .f = function(ts, channel_id, user, reply_users) {
      if (user %in% reply_users) {
        last_reply_user <- slackthreads::replies(ts, channel_id) %>%
          tibble::enframe() %>%
          tidyr::hoist(.data$value, "ts", "user") %>%
          dplyr::arrange(dplyr::desc(ts)) %>%
          dplyr::slice(1) %>%
          dplyr::pull(user)
        return(last_reply_user == user)
      } else {
        return(FALSE)
      }
    }
  )

  return(answerable)
}
