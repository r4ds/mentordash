#' The Main App Server Function
#'
#' @param input,output,session Shiny internal variables.
#'
#' @return Shiny reactive updates.
#' @keywords internal
.app_server <- function(input, output, session) {
  # CLEAN EVERYTHING UP TO ONLY DISPLAY WHEN QUESTIONS_DF SUCCEEDS.

  # Right now it does a weird double refresh, likely because modules aren't set
  # up properly. I'm merging to get things to a fixable state, though.

  # root_url <- .detect_root_url(session)
  # output$login_button <- shiny::renderUI(
  #   .login_button()
  # )

  query_string <- shiny::reactive(
    shiny::getQueryString(session)
  )

  output$app_ui <- shiny::renderUI(
    .ui_main()
  )

  question_channels <- shiny::reactive({
    # query <- shiny::getQueryString(session)
    shiny::req(query_string()$code)
    .get_question_channels(code = query_string()$code)
  })

  questions_df <- shiny::eventReactive(
    input$refresh,
    .get_questions(question_channels()),
    ignoreNULL = FALSE
  )

  output$question_table <- shiny::renderUI({
    shiny::req(questions_df())
    .question_table_output()
  })

  output$valuebox_answerable <- shinydashboard::renderValueBox(
    {
      count_answerable <- questions_df() %>%
        dplyr::filter(answerable) %>%
        nrow()

      shinydashboard::valueBox(
        count_answerable,
        subtitle = "Answerable Questions",
        icon = shiny::icon('hand-holding-heart'),
        color = 'aqua'
      )
    }
  )

  output$answerable_questions <- DT::renderDataTable({
    questions_df() %>%
      dplyr::filter(answerable) %>%
      dplyr::select(-answerable,-speech_balloon) %>%
      DT::datatable(rownames = FALSE,
                    # filter = 'top',
                    selection = 'single',
                    escape = FALSE)
    })

  output$valuebox_followup <- shinydashboard::renderValueBox({
    count_followup <- questions_df() %>%
      dplyr::filter(!answerable) %>%
      nrow()

    shinydashboard::valueBox(count_followup,
                             subtitle = "Waiting for OP Followup",
                             icon = shiny::icon('comment-dots'),
                             color = 'teal')
  })

  output$followup_questions <- DT::renderDataTable({
    questions_df() %>%
      dplyr::filter(!answerable) %>%
      dplyr::select(-answerable,-speech_balloon) %>%
      DT::datatable(rownames = FALSE,
                    # filter = 'top',
                    selection = 'single',
                    escape = FALSE)
  })

  output$refresh <- shiny::renderUI({
    shiny::req(questions_df())
    .refresh_button_output()
  })
}

.get_question_channels <- function(code) {
  slackteams::add_team_code(code, redirect_uri = root_url, verbose = FALSE)
  shiny::updateQueryString("?")
  slackteams::activate_team("R4ds")
  channels <- slackteams::get_team_channels()
  question_channels <- sort(
    grep('^help', channels$name[channels$is_channel], value = TRUE)
  )
  names(question_channels) <- question_channels
  return(question_channels)
}

#' Get R4DS Question Threads
#'
#' @return A tidy tibble of question information.
#' @keywords internal
.get_questions <- function(question_channels) {
  ## Read in Conversations ----
  convos <- purrr::map(
    question_channels,
    slackthreads::conversations
  )

  ## Response to Tibble ----
  convos_tbl <- .tidy_convos(convos)

  return(convos_tbl)
}

#' Rectangle Conversation Data
#'
#' @param convos A list returned by slackthreads::conversations.
#'
#' @return A tibble of question data.
#' @keywords internal
#' @importFrom rlang .data
.tidy_convos <- function(convos) {
  # For now I'm filtering stuff out here that's "done". Later we'll make these
  # explicit filters.

  bad_subtypes <- c("channel_join", "channel_name", "bot_add", "bot_message")

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
    tidyr::unnest_wider(.data$conversations) %>%
    # Get rid of channel_join and channel_name.
    dplyr::filter(
      !(.data$subtype %in% bad_subtypes)
    ) %>%
    dplyr::mutate(
      heavy_check_mark = .has_reaction(
        .data$reactions,
        c("heavy_check_mark", "question-answered")
      ),
      thread_tag = .has_reaction(
        .data$reactions,
        "thread"
      ),
      nevermind = .has_reaction(
        .data$reactions,
        c("question-nevermind", "octagonal_sign")
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
        channels = .data$channel, tses = .data$thread_ts
      )
    ) %>%
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
      excerpt = stringr::str_trunc(text,100),
      # links = paste(`web link`, `app link`, sep = " | "),
      latest_activity = as.POSIXct(
        purrr::map2_dbl(
          as.numeric(.data$ts), as.numeric(.data$latest_reply),
          max,
          na.rm = TRUE
        ),
        origin = "1970-01-01"
      ),
      latest_activity = format(latest_activity,"%Y-%m-%d %H:%M:%S")
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
                           channels,
                           tses) {
  purrr::pmap_lgl(
    .l = list(
      speech_balloon = speech_balloons,
      user = users,
      reply_users = reply_userses,
      channel = channels,
      ts = tses
    ),
    .f = function(speech_balloon, user, reply_users, channel, ts) {
      if (!speech_balloon) {
        return(TRUE)
      } else if (is.na(ts)) {
        return(TRUE)
      } else if (all(is.na(reply_users))) {
        return(TRUE)
      } else if (any(reply_users == user)) {
        # This is a stop-gap. Technically this could still be un-answerable
        # if they aren't the *latest* reply. We should grab the replies for
        # such posts and check, but maybe we'll grab them for *everything*.
        last_reply_user <- slackthreads::replies(ts, channel) %>%
          tibble::enframe() %>%
          tidyr::unnest_wider(value) %>%
          # dplyr::mutate(ts = as.numeric(ts)) %>%
          dplyr::arrange(dplyr::desc(ts)) %>%
          dplyr::slice(1) %>%
          dplyr::pull(user)
        return(last_reply_user == user)
      } else {
        return(FALSE)
      }
    }
  )
}

# This will eventually be used to auto-set the return url for the login.
.detect_root_url <- function(session) {
  if (shiny::isolate(session$clientData$url_hostname) == "127.0.0.1") {
    return("http://127.0.0.1:4242")
  } else {
    return("https://r4dscommunity.shinyapps.io/mentordash")
  }
}
