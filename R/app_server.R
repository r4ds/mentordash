#' The Main App Server Function
#'
#' @param input,output,session Shiny internal variables.
#'
#' @return Shiny reactive updates.
#' @keywords internal
.app_server <- function(input, output,session) {
  questions_df <- .get_questions()

  output$questions <- .render_questions(questions_df)
  session$onSessionEnded(shiny::stopApp)
}

#' Get R4DS Question Threads
#'
#' @return A tidy tibble of question information.
#' @keywords internal
.get_questions <- function() {
  # Eventually the question list should probably be a reactivePoll of some sort.
  # For now it will just load once.
  slackteams::load_team_dcf("r4ds")
  slackteams::activate_team("r4ds")
  channels <- slackteams::get_team_channels()
  question_channels <- sort(
    grep('^help_', channels$name[channels$is_channel], value = TRUE)
  )
  names(question_channels) <- question_channels

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
.tidy_convos <- function(convos) {
  # For now I'm filtering stuff out here that's "done". Later we'll make these
  # explicit filers.

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
    tidyr::unnest_wider(conversations) %>%
    # Get rid of channel_join and channel_name.
    dplyr::filter(
      !(subtype %in% c("channel_join", "channel_name"))
    ) %>%
    dplyr::mutate(
      heavy_check_mark = .has_reaction(reactions, "heavy_check_mark"),
      thread_tag = .has_reaction(reactions, "thread")
    ) %>%
    dplyr::filter(!heavy_check_mark, !thread_tag) %>%
    dplyr::mutate(
      speech_balloon = .has_reaction(reactions, "speech_balloon"),
      answerable = .is_answerable(speech_balloon, user, replies)
    ) %>%
    dplyr::filter(answerable) %>%
    dplyr::mutate(
      `web link` = purrr::map2(channel_id, ts, function(chnl, this_ts) {
        paste0(
          "<a href=\"",
          paste(
            "https://app.slack.com/client/T6UC1DKJQ",
            chnl,
            this_ts,
            sep = "/"
          ),
          "\">link</a>"
        )
      }),
      `app link` = purrr::map2(channel_id, ts, function(chnl, this_ts) {
        paste0(
          "<a href=\"",
          paste(
            "https://rfordatascience.slack.com/archives",
            chnl,
            paste0("p", sub(x = this_ts, "\\.", "")),
            sep = "/"
          ),
          "\">link</a>"
        )
      }),
      latest_activity = as.POSIXct(
        purrr::map2_dbl(
          as.numeric(ts), as.numeric(latest_reply),
          max,
          na.rm = TRUE
        ),
        origin = "1970-01-01"
      )
    ) %>%
    dplyr::select(
      channel, `web link`, `app link`, latest_activity
    ) %>%
    dplyr::arrange(dplyr::desc(latest_activity))

  return(convos_tbl)
}

#' Check for a Reaction in Reactions
#'
#' @param reactions A list of reaction lists.
#' @param reaction_name A character scalar with the name of the target reaction.
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
            rxn$name == reaction_name
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
#' @param replieses The list of lists of replies to each message.
#'
#' @return A logical vector.
#' @keywords internal
.is_answerable <- function(speech_balloons, users, replieses) {
  purrr::pmap_lgl(
    .l = list(
      speech_balloon = speech_balloons,
      user = users,
      replies = replieses
    ),
    .f = function(speech_balloon, user, replies) {
      if (!speech_balloon) {
        return(TRUE)
      } else {
        if (all(is.na(replies))) {
          return(TRUE)
        } else {
          if (dplyr::last(replies)$user == user) {
            return(TRUE)
          } else {
            return(FALSE)
          }
        }
      }
    }
  )
}

#' Render the Questions DataTable
#'
#' @param df A dataframe of questions.
#'
#' @return A \code{\link[DT]{renderDataTable}}.
#' @keywords internal
.render_questions <- function(df) {
  shiny::callModule(
    module = function(input, output, server) {
      DT::renderDataTable({
        df
      }, escape = FALSE)
    },
    id = "questions"
  )
}
