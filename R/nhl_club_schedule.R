#' @title **NHL Club Schedule**
#' @description Returns schedule data for a given team. Supports season, month,
#' and week views.
#' @param team_abbr Three-letter team abbreviation (e.g., "TOR", "BOS")
#' @param season Integer 4-digit *end year* of the season (e.g., 2026 for
#'   the 2025-26 season), matching [most_recent_nhl_season()]. If NULL,
#'   returns the current season schedule.
#' @param month Character month in "YYYY-MM" format (e.g., "2025-01").
#'   If provided, returns that month's schedule. Ignored if view is "week".
#' @param view Character: "season" (default), "month", or "week".
#' @param date Character date in "YYYY-MM-DD" format for week view.
#'   If NULL with view="week", returns current week.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                                      |types     |description                                       |
#'    |:---------------------------------------------|:---------|:-------------------------------------------------|
#'    |id                                            |integer   |Unique game identifier.                           |
#'    |season                                        |integer   |Season in 8-digit API format (e.g., 20252026).    |
#'    |game_type                                     |integer   |Game type code (1 preseason, 2 regular, 3 playoff).|
#'    |game_date                                     |character |Game date.                                        |
#'    |neutral_site                                  |logical   |Whether the game is at a neutral site.            |
#'    |start_time_utc                                |character |Scheduled start time in UTC.                      |
#'    |eastern_utc_offset                            |character |Eastern time UTC offset.                          |
#'    |venue_utc_offset                              |character |Venue UTC offset.                                 |
#'    |venue_timezone                                |character |Venue time zone.                                  |
#'    |game_state                                    |character |Current game state.                               |
#'    |game_schedule_state                           |character |Schedule state of the game.                       |
#'    |tv_broadcasts                                 |list      |Nested list of TV broadcast details.              |
#'    |three_min_recap                               |character |Link to the three-minute recap.                   |
#'    |game_center_link                              |character |Link to the game center page.                     |
#'    |three_min_recap_fr                            |character |Link to the French three-minute recap.            |
#'    |condensed_game_fr                             |character |Link to the French condensed game video.          |
#'    |condensed_game                                |character |Link to the condensed game video.                 |
#'    |venue_default                                 |character |Venue name (default language).                    |
#'    |venue_fr                                      |character |Venue name (French).                              |
#'    |venue_es                                      |character |Venue name (Spanish).                             |
#'    |away_team_id                                  |integer   |Away team identifier.                             |
#'    |away_team_abbrev                              |character |Away team abbreviation.                           |
#'    |away_team_logo                                |character |URL to the away team logo.                        |
#'    |away_team_dark_logo                           |character |URL to the away team dark logo.                   |
#'    |away_team_away_split_squad                    |logical   |Whether the away team is a split squad.           |
#'    |away_team_score                               |integer   |Away team score.                                  |
#'    |away_team_hotel_link                          |character |Link to away team hotel info.                     |
#'    |away_team_hotel_desc                          |character |Away team hotel description.                       |
#'    |away_team_airline_link                        |character |Link to away team airline info.                   |
#'    |away_team_airline_desc                        |character |Away team airline description.                     |
#'    |away_team_common_name_default                 |character |Away team common name (default language).         |
#'    |away_team_common_name_fr                      |character |Away team common name (French).                   |
#'    |away_team_place_name_default                  |character |Away team place name (default language).          |
#'    |away_team_place_name_fr                       |character |Away team place name (French).                    |
#'    |away_team_place_name_with_preposition_default |character |Away team place name with preposition (default).  |
#'    |away_team_place_name_with_preposition_fr      |character |Away team place name with preposition (French).   |
#'    |home_team_id                                  |integer   |Home team identifier.                             |
#'    |home_team_abbrev                              |character |Home team abbreviation.                           |
#'    |home_team_logo                                |character |URL to the home team logo.                        |
#'    |home_team_dark_logo                           |character |URL to the home team dark logo.                   |
#'    |home_team_home_split_squad                    |logical   |Whether the home team is a split squad.           |
#'    |home_team_score                               |integer   |Home team score.                                  |
#'    |home_team_hotel_link                          |character |Link to home team hotel info.                     |
#'    |home_team_hotel_desc                          |character |Home team hotel description.                       |
#'    |home_team_airline_link                        |character |Link to home team airline info.                   |
#'    |home_team_airline_desc                        |character |Home team airline description.                     |
#'    |home_team_common_name_default                 |character |Home team common name (default language).         |
#'    |home_team_common_name_fr                      |character |Home team common name (French).                   |
#'    |home_team_place_name_default                  |character |Home team place name (default language).          |
#'    |home_team_place_name_fr                       |character |Home team place name (French).                    |
#'    |home_team_place_name_with_preposition_default |character |Home team place name with preposition (default).  |
#'    |home_team_place_name_with_preposition_fr      |character |Home team place name with preposition (French).   |
#'    |period_descriptor_period_type                 |character |Period type descriptor.                           |
#'    |period_descriptor_max_regulation_periods      |integer   |Maximum number of regulation periods.             |
#'    |game_outcome_last_period_type                 |character |Period type in which the game ended.              |
#'    |winning_goalie_player_id                      |integer   |Winning goalie player identifier.                 |
#'    |winning_goalie_first_initial_default          |character |Winning goalie first initial (default language).  |
#'    |winning_goalie_last_name_default              |character |Winning goalie last name (default language).      |
#'    |winning_goalie_last_name_cs                   |character |Winning goalie last name (Czech).                 |
#'    |winning_goalie_last_name_fi                   |character |Winning goalie last name (Finnish).               |
#'    |winning_goalie_last_name_sk                   |character |Winning goalie last name (Slovak).                |
#'    |winning_goalie_last_name_sv                   |character |Winning goalie last name (Swedish).               |
#'    |winning_goal_scorer_player_id                 |integer   |Winning goal scorer player identifier.            |
#'    |winning_goal_scorer_first_initial_default     |character |Winning goal scorer first initial (default).      |
#'    |winning_goal_scorer_last_name_default         |character |Winning goal scorer last name (default language). |
#'    |winning_goal_scorer_last_name_cs              |character |Winning goal scorer last name (Czech).            |
#'    |winning_goal_scorer_last_name_sk              |character |Winning goal scorer last name (Slovak).           |
#'    |winning_goal_scorer_last_name_fi              |character |Winning goal scorer last name (Finnish).          |
#'    |team_abbr                                     |character |Team abbreviation queried.                        |
#' @keywords NHL Club Schedule
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_club_schedule(team_abbr = "TOR"))
#' }
nhl_club_schedule <- function(
    team_abbr,
    season = NULL,
    month = NULL,
    view = "season",
    date = NULL
) {
    if (view == "week") {
        if (is.null(date)) {
            url <- glue::glue(
                "https://api-web.nhle.com/v1/club-schedule/{team_abbr}/week/now"
            )
        } else {
            url <- glue::glue(
                "https://api-web.nhle.com/v1/club-schedule/{team_abbr}/week/{date}"
            )
        }
    } else if (view == "month") {
        if (is.null(month)) {
            url <- glue::glue(
                "https://api-web.nhle.com/v1/club-schedule/{team_abbr}/month/now"
            )
        } else {
            url <- glue::glue(
                "https://api-web.nhle.com/v1/club-schedule/{team_abbr}/month/{month}"
            )
        }
    } else {
        # season view
        if (is.null(season)) {
            url <- glue::glue(
                "https://api-web.nhle.com/v1/club-schedule-season/{team_abbr}/now"
            )
        } else {
            # `season` is the end year (e.g. 2026 = 2025-26)
            api_season <- paste0(season - 1, season)
            url <- glue::glue(
                "https://api-web.nhle.com/v1/club-schedule-season/{team_abbr}/{api_season}"
            )
        }
    }

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            games <- raw$games
            if (is.null(games) || length(games) == 0) {
                message(glue::glue(
                    "{Sys.time()}: No schedule data for {team_abbr}"
                ))
                return(NULL)
            }

            if (is.data.frame(games)) {
                df <- games
            } else {
                df <- jsonlite::fromJSON(
                    jsonlite::toJSON(games, auto_unbox = TRUE),
                    flatten = TRUE
                )
            }

            df$team_abbr <- team_abbr
            df <- df %>%
                janitor::clean_names() %>%
                make_fastRhockey_data("NHL Club Schedule", Sys.time())

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching club schedule for {team_abbr}: {e$message}"
            ))
            return(NULL)
        }
    )
}
