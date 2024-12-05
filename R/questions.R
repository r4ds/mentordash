.get_raw_questions <- function(question_channels,
                               results_per_channel,
                               slack_api_key) {
  convos <- purrr::map(
    question_channels,
    slackthreads::conversations,
    max_results = results_per_channel,
    limit = min(results_per_channel, 1000L),
    token = slack_api_key
  )
  # cli::cli_inform("Got questions.")
  return(convos)
}


#' Check Whether a Question is Answerable
#'
#' @inheritParams .tidy_convos
#' @param speech_balloons The logical vector indicating whether a question is
#'   tagged with the "needs more information" emoji.
#' @param users The character vector of users who posted the question.
#' @param reply_counts The integer vector of numbers of replies to each message.
#' @param reply_userses The list of character vectors of users who have replied
#'   to this thread.
#' @param channel_ids The character vector of channels in which the questions
#'   were posted.
#' @param tses The character vector of timestamps for threads attached to the
#'   question.
#' @param latest_activities The character vector of timestamps of the most
#'   recent activity on each post.
#'
#' @return A logical vector.
#' @keywords internal
.is_answerable <- function(speech_balloons,
                           users,
                           reply_counts,
                           reply_userses,
                           channel_ids,
                           tses,
                           latest_activities,
                           slack_api_key) {
  # We need to return a logical vector indicating whether this question is
  # "answerable", meaning the thread hasn't been tagged as needing more info OR
  # there isn't a reply OR the latest reply was by the user. To determine the
  # latest reply, we need to do an extra slack call, so try to do that as little
  # as possible. And try to vectorize this cleanly.

  answerable <- !speech_balloons | is.na(tses)

  # Test the FALSEs.

  # If reply_count is 0, it's answerable. This shouldn't happen, because it
  # implies there's a thread ts but no replies.
  answerable[!answerable] <- purrr::map_lgl(
    reply_counts[!answerable],
    `!`
  )

  # We need to check if the op was the *most recent* reply. This can be slow.
  answerable[!answerable] <- purrr::pmap_lgl(
    list(
      ts = tses[!answerable],
      channel_id = channel_ids[!answerable],
      user = users[!answerable],
      reply_users = reply_userses[!answerable],
      latest_activity = latest_activities[!answerable]
    ),
    function(ts, channel_id, user, reply_users, latest_activity) {
      if (user %in% reply_users) {
        latest_reply_user <- .get_latest_reply_user(
          ts,
          channel_id,
          latest_activity,
          slack_api_key = slack_api_key
        )
        return(latest_reply_user == user)
      }
      return(FALSE)
    }
  )

  return(answerable)
}

.get_latest_reply_user <- function(ts,
                                   channel_id,
                                   latest_activity,
                                   slack_api_key) {
  thread_key <- paste(channel_id, ts, sep = "_")
  latest_reply_user <- rlang::env_cache(
    the,
    thread_key,
    .fetch_latest_reply_user(ts, channel_id, slack_api_key)
  )

  if (latest_reply_user$ts != latest_activity) {
    rlang::env_bind(
      the,
      !!thread_key := .fetch_latest_reply_user(ts, channel_id, slack_api_key)
    )
  }
  return(the[[thread_key]]$user)
}

.fetch_latest_reply_user <- function(ts, channel_id, slack_api_key) {
  slackthreads::replies(ts, channel_id, token = slack_api_key) |>
    tibble::enframe(name = NULL) |>
    tidyr::hoist(.data$value, "ts", "user") |>
    dplyr::arrange(dplyr::desc(.data$ts)) |>
    dplyr::slice(1) |>
    dplyr::select("ts", "user")
}

#' Check for a Reaction in Reactions
#'
#' @param reactions A list of reaction lists.
#' @param reaction_name A character vector with the name of one or more target
#'   reactions.
#' @param users The character vector of user ids of the person who posted each
#'   message.
#' @param mentors A character vector of mentors to use for filtering.
#'
#' @return A logical vector the same length as reactions.
#' @keywords internal
.has_reaction <- function(reactions,
                          reaction_name,
                          users = character(length(reactions)),
                          mentors = character()) {
  purrr::map2_lgl(
    reactions, users,
    function(rxns, user) {
      if (all(is.na(rxns))) {
        return(FALSE)
      }
      any(
        purrr::map_lgl(
          rxns,
          function(rxn) {
            rxn$name %in% reaction_name &&
              (
                !length(mentors) ||
                  user %in% rxn$users ||
                  any(rxn$users %in% mentors)
              )
          }
        )
      )
    }
  )
}

.rectangle_convos <- function(convos) {
  purrr::map_dfr(
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
  ) |>
    # A general unnest_wider was breaking, so let's just get the columns we
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
    ) |>
    dplyr::mutate(
      reply_count = tidyr::replace_na(.data$reply_count, 0)
    )
}
.simple_filter_convos_table <- function(convos_tbl, mentors) {
  bad_subtypes <- c("channel_join", "channel_name", "bot_add", "bot_message")
  # keep_timeframe <- lubridate::weeks(3)
  keep_timeframe <- lubridate::years(1)

  convos_tbl <- convos_tbl |>
    # Get rid of channel_join and channel_name.
    dplyr::filter(
      !(.data$subtype %in% bad_subtypes),
      .data$user != "USLACKBOT"
    ) |>
    # Only keep "recent" threads.
    dplyr::mutate(
      latest_activity_raw = purrr::map2_chr(
        .data$ts,
        .data$latest_reply,
        function(ts, latest_reply) {
          # I want to keep the string, but compare them numerically.
          latest_numeric <- as.numeric(latest_reply)
          if (is.na(latest_numeric) || as.numeric(ts) > latest_numeric) {
            return(ts)
          }
          return(latest_reply)
        }
      ),
      latest_activity = as.POSIXct(
        as.numeric(.data$latest_activity_raw),
        origin = "1970-01-01"
      )
    ) |>
    dplyr::filter(
      .data$latest_activity >= (lubridate::now() - keep_timeframe)
    ) |>
    dplyr::mutate(
      heavy_check_mark = .has_reaction(
        .data$reactions,
        c("heavy_check_mark", "question-answered", "white_check_mark"),
        users = .data$user,
        mentors = mentors
      ),
      thread_tag = .has_reaction(
        .data$reactions,
        c("thread", "reply"),
        mentors = mentors
      ),
      nevermind = .has_reaction(
        .data$reactions,
        c("question-nevermind", "octagonal_sign", "nevermind"),
        users = .data$user,
        mentors = mentors
      )
    ) |>
    dplyr::filter(
      !.data$heavy_check_mark,
      !.data$thread_tag,
      !.data$nevermind
    )
  # cli::cli_inform("Applied quick filters")
  return(convos_tbl)
}

.tag_answerable <- function(convos_tbl, mentors, slack_api_key) {
  convos_tbl <- convos_tbl |>
    dplyr::mutate(
      speech_balloon = .has_reaction(
        .data$reactions,
        c("speech_balloon", "question-more-info"),
        .data$user,
        mentors
      )
    )

  convos_tbl <- convos_tbl |>
    dplyr::mutate(
      answerable = .is_answerable(
        .data$speech_balloon,
        .data$user,
        .data$reply_count,
        .data$reply_users,
        channel_ids = .data$channel_id,
        tses = .data$thread_ts,
        latest_activities = .data$latest_activity_raw,
        slack_api_key = slack_api_key
      )
    )
  # cli::cli_inform("Tagged answerable.")
  return(convos_tbl)
}

.finish_convos_table <- function(convos_tbl) {
  convos_tbl <- convos_tbl |>
    dplyr::mutate(
      link = purrr::map2(
        .data$channel_id, .data$ts,
        function(chnl, this_ts) {
          paste0(
            "<a href=\"",
            paste(
              "https://dslcio.slack.com/archives",
              chnl,
              paste0("p", sub(x = this_ts, "\\.", "")),
              sep = "/"
            ),
            "\", target=\"_blank\">load question</a>"
          )
        }
      ),
      excerpt = stringr::str_trunc(.data$text, 100),
      latest_activity = format(.data$latest_activity, "%Y-%m-%d %H:%M:%S")
    ) |>
    dplyr::select(
      "channel",
      "excerpt",
      "reply_count",
      "speech_balloon",
      "answerable",
      "link",
      "latest_activity"
    ) |>
    dplyr::arrange(.data$latest_activity)
  # cli::cli_inform("Finished convos table.")
  return(convos_tbl)
}
