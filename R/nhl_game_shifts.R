#' @title **NHL Game Shifts**
#' @description Returns information on game shifts for a given game id
#' @param game_id Game unique ID
#' @return A data frame (`fastRhockey_data`) with one row per shift change and
#' the following columns:
#'
#'    |col_name               |types     |description                              |
#'    |:----------------------|:---------|:----------------------------------------|
#'    |event_team             |character |Team making the line change.             |
#'    |period                 |integer   |Period number.                           |
#'    |period_time            |character |Time in the period of the change (MM:SS).|
#'    |period_seconds         |numeric   |Seconds elapsed in the period.           |
#'    |game_seconds           |numeric   |Seconds elapsed in the game.             |
#'    |num_on                 |integer   |Number of players coming on the ice.     |
#'    |players_on             |character |Names of players coming on the ice.      |
#'    |ids_on                 |character |Player IDs coming on the ice.            |
#'    |num_off                |integer   |Number of players going off the ice.     |
#'    |players_off            |character |Names of players going off the ice.      |
#'    |ids_off                |character |Player IDs going off the ice.            |
#'    |event                  |character |Event label ("Change").                  |
#'    |event_type             |character |Event type code ("CHANGE").              |
#'    |game_seconds_remaining |numeric   |Seconds remaining in the game.           |
#' @keywords NHL Game Shifts
#' @import rvest
#' @importFrom rlang .data
#' @importFrom lubridate ms period_to_seconds
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'   try(nhl_game_shifts(game_id = 2021020182))
#' }
nhl_game_shifts <- function(game_id){

  base_url <- "https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId="

  full_url <- paste0(base_url,
                     game_id)
  res <- httr::RETRY("GET", full_url)

  # Check the result
  check_status(res)

  # Default return so the outer `return(shifts)` cannot error with
  # "object 'shifts' not found" if the body below short-circuits or errors.
  shifts <- NULL

  tryCatch(
    expr = {
      resp <- res %>%
        httr::content(as = "text", encoding = "UTF-8")
      site <- jsonlite::fromJSON(resp)

      # The endpoint is no longer populated for a growing share of
      # 2024-25 and 2025-26 games (~38% of 2025-26 as of late 2026).
      # When the API returns `{total: 0, data: []}`, short-circuit with
      # an empty tibble rather than letting the dplyr pipeline error
      # on missing columns and trip the misleading "no game shifts
      # data for X available!" message below.
      data_empty <- is.null(site$data) ||
        (is.data.frame(site$data) && nrow(site$data) == 0) ||
        (is.list(site$data) && length(site$data) == 0)

      if (data_empty) {
        shifts <- tibble::tibble()
      } else {
      shifts_raw <- site$data %>%
        dplyr::tibble() %>%
        janitor::clean_names() %>%
        tidyr::unite("player_name", c("first_name", "last_name"), sep = " ") %>%
        dplyr::select("game_id", "player_id", "player_name", "team_abbrev", "team_id",
                      "team_name", "period", "start_time", "end_time", "duration") %>%
        dplyr::filter(!is.na(.data$duration)) %>%
        dplyr::mutate(
          start_time_ms = lubridate::ms(.data$start_time),
          start_seconds = lubridate::period_to_seconds(.data$start_time_ms),
          start_game_seconds = .data$start_seconds + (1200 * (.data$period-1)),
          end_time_ms = lubridate::ms(.data$end_time),
          end_seconds = lubridate::period_to_seconds(.data$end_time_ms),
          end_game_seconds = .data$end_seconds + (1200 * (.data$period-1)),
          duration = lubridate::ms(.data$duration),
          duration_seconds = lubridate::period_to_seconds(.data$duration)
        ) %>%
        dplyr::filter(.data$duration_seconds > 0)

      shifts_on <- shifts_raw %>%
        dplyr::group_by(
          .data$team_name, .data$period, .data$start_time, .data$start_seconds, .data$start_game_seconds
        ) %>%
        dplyr::summarize(
          num_on = dplyr::n(),
          players_on = paste(.data$player_name, collapse = ", "),
          ids_on = paste(.data$player_id, collapse = ", "),
          .groups = "drop"
        ) %>%
        dplyr::rename(
          "period_time" = "start_time",
          "period_seconds" = "start_seconds",
          "game_seconds" = "start_game_seconds"
        )

      shifts_off <- shifts_raw %>%
        dplyr::group_by(
          .data$team_name, .data$period, .data$end_time, .data$end_seconds, .data$end_game_seconds
        ) %>%
        dplyr::summarize(
          num_off = dplyr::n(),
          players_off = paste(.data$player_name, collapse = ", "),
          ids_off = paste(.data$player_id, collapse = ", "),
          .groups = "drop"
        ) %>%
        dplyr::rename(
          "period_time" = "end_time",
          "period_seconds" = "end_seconds",
          "game_seconds" = "end_game_seconds"
        )

      shifts <- dplyr::full_join(
        shifts_on, shifts_off,
        by = c("game_seconds", "team_name", "period", "period_time", "period_seconds")
      ) %>%
        dplyr::arrange(.data$game_seconds) %>%
        dplyr::mutate(
          event = "Change",
          event_type = "CHANGE",
          game_seconds_remaining = 3600 - .data$game_seconds
        ) %>%
        dplyr::rename("event_team" = "team_name") %>%
        # removing NA values at start and end of periods
        dplyr::mutate(
          players_on = ifelse(is.na(.data$players_on), "None", .data$players_on),
          players_off = ifelse(is.na(.data$players_off), "None", .data$players_off),
          ids_on = ifelse(is.na(.data$ids_on), 0, .data$ids_on),
          ids_off = ifelse(is.na(.data$ids_off), 0, .data$ids_off)
        ) %>%
        make_fastRhockey_data("NHL Game Shifts Information from NHL.com",Sys.time())
      } # end else (non-empty branch)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no game shifts data for {game_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(shifts)
}
