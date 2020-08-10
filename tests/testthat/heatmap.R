library(magrittr)
devtools::load_all(".")

slackteams::load_teams()
slackteams::activate_team("r4ds")
channels <- slackteams::get_team_channels()
question_channels <- sort(
  grep("^help", channels$name[channels$is_channel], value = TRUE)
)
names(question_channels) <- question_channels

convos <- purrr::map(
  question_channels,
  slackthreads::conversations
)

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
    thread_tag = .has_reaction(
      .data$reactions,
      "thread"
    )
  ) %>%
  dplyr::filter(!thread_tag) %>%
  dplyr::mutate(
    posted_at = as.POSIXct(
      as.numeric(.data$ts),
      origin = "1970-01-01"
    ),
    posted_at = format(posted_at, "%Y-%m-%d %H:%M:%S")
  )

# convos_tbl %>%
#   dplyr::glimpse()

convos_tbl %>%
  dplyr::mutate(
    posted_day = lubridate::wday(posted_at, label = TRUE),
    posted_hour = lubridate::hour(posted_at)
  ) %>%
  dplyr::select(
    channel, posted_day, posted_hour
  ) %>%
  dplyr::count(posted_day, posted_hour, sort = TRUE) %>%
  dplyr::pull(posted_day) %>%
  head(1) %>%
  dput()

days <- factor(
  c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
  levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
  ordered = TRUE
)

all_days <- tibble::tibble(
  posted_day = rep(days, 24)
) %>%
  dplyr::arrange(posted_day) %>%
  dplyr::mutate(posted_hour = rep(0:23, 7))

heatmap <- convos_tbl %>%
  dplyr::mutate(
    posted_day = lubridate::wday(posted_at, label = TRUE),
    posted_hour = lubridate::hour(posted_at)
  ) %>%
  dplyr::select(
    channel, posted_day, posted_hour
  ) %>%
  dplyr::count(posted_day, posted_hour, sort = TRUE) %>%
  dplyr::right_join(all_days) %>%
  tidyr::replace_na(list(n = 0L)) %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = posted_day, y = posted_hour, fill = n) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_viridis_c() +
  ggplot2::theme_minimal()
