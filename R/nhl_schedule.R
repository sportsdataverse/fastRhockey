#' @title **NHL Schedule**
#' @description Returns NHL Schedule data
#' @param season NHL Season
#' @param day Date
#' @return Returns a tibble
#' @keywords NHL Schedule
#' @import rvest
#' @importFrom rlang .data
#' @importFrom lubridate ms period_to_seconds
#' @importFrom jsonlite fromJSON toJSON read_json
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows tibble
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'   nhl_schedule(season = 2021)
#' }
nhl_schedule <- function(season = NULL, day = as.Date(Sys.Date(), "%Y-%m-%d")){

  if(is.null(season)){
    # scrape day's games
    url <- glue::glue("https://statsapi.web.nhl.com/api/v1/schedule?date={day}")

    site <- jsonlite::read_json(url)

    if(site$totalGames == 0){
      message(glue::glue("No NHL games found on {day}"))
    }

  } else {
    # scrape season's games
    if(season == 2020){
      # searching the nhl api for games between Sep 1 2019 & Sep 30th 2020
      url <- glue::glue("https://statsapi.web.nhl.com/api/v1/schedule?startDate={season-1}-09-01&endDate={season}-09-30")
    } else {
      # searching the nhl api for games between Sep 1 & July 31
      url <- glue::glue("https://statsapi.web.nhl.com/api/v1/schedule?startDate={season-1}-09-01&endDate={season}-07-31")
    }

    site <- jsonlite::read_json(url)
  }


  games <- jsonlite::fromJSON(jsonlite::toJSON(site[["dates"]]), flatten=TRUE) %>%
    dplyr::tibble()
  game_dates <- data.table::rbindlist(games$games, fill = TRUE)
  game_dates <- game_dates %>%
    janitor::clean_names()
  select_cols <- as.vector(colnames(game_dates)[!stringr::str_detect(colnames(game_dates),"league(.*)")])
  game_dates <- game_dates %>%
    dplyr::select(dplyr::all_of(select_cols))
  colnames(game_dates) <- gsub("teams_","",colnames(game_dates))
  game_dates <- game_dates %>%
    dplyr::rename(
      game_id = .data$game_pk,
      season_full = .data$season,
      game_type_abbreviation = .data$game_type,
      game_date_time = .data$game_date) %>%
    dplyr::mutate(
      game_type = dplyr::case_when(
        substr(.data$game_id, 6, 6) == 1 ~ "PRE",
        substr(.data$game_id, 6, 6) == 2 ~ "REG",
        substr(.data$game_id, 6, 6) == 3 ~ "POST",
        substr(.data$game_id, 6, 6) == 4 ~ "ALLSTAR"),
      venue_id = ifelse(.data$venue_id == "NULL", NA_integer_, .data$venue_id),
      game_date = as.Date(substr(.data$game_date_time,1,10),"%Y-%m-%d"))

  game_dates <- game_dates %>%
    dplyr::filter(.data$game_type == "REG" | .data$game_type == "POST")

  # make sure we're only pulling for correct season by using
  # the season code in the game_id

  if(!is.null(season)) {
    game_dates <- game_dates %>%
      dplyr::filter(substr(.data$game_id, 1, 4) == (as.numeric(season) - 1))
  }


  return(game_dates)
}
