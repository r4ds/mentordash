.app_server <- function(input, output,session) {
  questions_df <- .get_questions()

  output$questions <- .render_questions(questions_df)
}

.get_questions <- function() {
  # Eventually the question list should probably be a reactivePoll of some sort.
  # For now it will just load once.
  slackteams::load_teams()
  slackteams::activate_team('r4ds')
  channels <- slackteams::get_team_channels()
  question_channels <- sort(
    grep('^[1-9]', channels$name[channels$is_channel], value = TRUE)
  )
  question_channels <- setNames(question_channels, question_channels)
  ## Read in Conversations ----
  convos <- lapply(question_channels, function(channel) {
    slackthreads::conversations(channel = channel)
  })
  ## Response to Tibble ----
  convos_tbl <- purrr::map_df(convos, .tidy_convo, .id = 'channel') %>%
    dplyr::mutate(
      latest_activity = max(
        latest_reply_date,
        root_date,
        na.rm = TRUE
      ),
      # Fetching the permalink is slow. Do this dynamically as needed/only for
      # the first few?
      permalink = purrr::map2(channel, ts_root, get_permalink)
    ) %>%
    dplyr::select(
      channel,
      permalink,
      root_date,
      latest_activity,
      heavy_check_mark,
      speech_balloon
    )
  return(convos_tbl)
}

get_permalink <- function(channel,
                          timestamp,
                          api_token = Sys.getenv("SLACK_API_TOKEN")) {
  # Migrate this to slackthreads.
  if ( !is.character(channel) | length(channel) > 1 ) { stop("channel must be a character vector of length one") }
  if ( !is.character(api_token) | length(api_token) > 1 ) { stop("api_token must be a character vector of length one") }

  chnl_map <- slackteams::get_team_channels()[,c('id','name')]
  this_chnl <- chnl_map$id[grepl(channel,chnl_map$name)]

  resp <- httr::POST(url = "https://slack.com/api/chat.getPermalink",
                     body = list(token = api_token,
                               channel = this_chnl,
                               message_ts = timestamp))
  httr::warn_for_status(resp)

  if (is.null(httr::content(resp)$permalink)) {
    return(NA_character_)
  } else {
    return(httr::content(resp)$permalink)
  }
}

.render_questions <- function(df) {
  shiny::callModule(
    module = function(input, output, server) {
      DT::renderDataTable({
        df
      })
    },
    id = "questions"
  )
}

.tidy_convo <- function(convo){
  y <- purrr::map_df(convo,.f=function(x) {
    if(is.null(x$client_msg_id))
      x$client_msg_id <- NA_character_

    if(is.null(x$reply_count))
      x$reply_count <- NA_integer_

    tibble::tibble(
      id = x$client_msg_id,
      ts_root= x$ts,
      root = x$user,
      root_text = x$text,
      reply_count = x$reply_count,
      reply_users = list(x$reply_users),
      replies = list(x$replies),
      latest_reply = list(x$latest_reply),
      reactions = list(x$reactions))
  })

  y$latest_reply <- purrr::map_chr(y$latest_reply,.f = function(x){
    if(is.null(x))
      return(NA_character_)

    x
  })

  y$replies <- purrr::modify(y$replies,.f=function(x){
    if(is.null(x)){
      ret <- tibble::tibble(user = NA_character_,ts = NA_character_)
    }else{
      ret <- tidyr::unnest(tibble::as_tibble(purrr::transpose(x)), cols = c(user, ts))
    }

    ret$reply_date <- as.POSIXct(as.numeric(ret$ts), origin="1970-01-01")
    ret
  })

  y$root_date <- as.POSIXct(as.numeric(y$ts_root), origin="1970-01-01")

  y$latest_reply_date <- as.POSIXct(as.numeric(y$latest_reply), origin="1970-01-01")

  y$heavy_check_mark <- purrr::map(y$reactions,.f = function(rxn) {

    if(is.null(rxn))
      return(FALSE)

    purrr::map_lgl(rxn,.f=function(rxn_i){"heavy_check_mark"%in%rxn_i$name})

  })

  y$heavy_check_mark <- purrr::map_lgl(y$heavy_check_mark,any)

  y$speech_balloon <- purrr::map(y$reactions,.f = function(rxn) {

    if(is.null(rxn))
      return(FALSE)

    purrr::map_lgl(rxn,.f=function(rxn_i){"speech_balloon"%in%rxn_i$name})

  })

  y$speech_balloon <- purrr::map_lgl(y$speech_balloon,any)

  y$duration <- NA_real_

  y$duration[y$heavy_check_mark] <- lubridate::make_difftime(y$latest_reply_date[y$heavy_check_mark] - y$root_date[y$heavy_check_mark],units = 'days')


  y$duration[!y$heavy_check_mark] <- lubridate::make_difftime(Sys.time() - y$root_date[!y$heavy_check_mark],units = 'days')

  y$duration[is.na(y$latest_reply)&y$heavy_check_mark] <- lubridate::make_difftime(Sys.Date() - Sys.Date(),units = 'days')

  y
}
