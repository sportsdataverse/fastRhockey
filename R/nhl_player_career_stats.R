#' @title **NHL Player Career Stats**
#' @description Aggregator helper that combines biographical information
#'   from the NHL player landing endpoint with the player's season-by-season
#'   career totals. Returns a single multi-row "career-by-season" data frame.
#'   Mirrors the `Stats.player_career_stats` convenience helper from the
#'   `nhl-api-py` Python client.
#' @param player_id Integer player ID (e.g. `8478402` for Connor McDavid).
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                                 |types     |description                              |
#'    |:----------------------------------------|:---------|:----------------------------------------|
#'    |player_id                                |integer   |Unique player identifier.                |
#'    |first_name                               |character |Player first name.                       |
#'    |last_name                                |character |Player last name.                        |
#'    |position                                 |character |Player position.                         |
#'    |assists                                  |integer   |Assists.                                 |
#'    |game_type_id                             |integer   |Game type the row belongs to.            |
#'    |games_played                             |integer   |Games played.                            |
#'    |goals                                    |integer   |Goals scored.                            |
#'    |league_abbrev                            |character |League abbreviation.                     |
#'    |pim                                      |integer   |Penalty minutes.                         |
#'    |points                                   |integer   |Total points (goals + assists).          |
#'    |season                                   |integer   |Season (concluding year, YYYY).          |
#'    |sequence                                 |integer   |Sequence order of the season row.        |
#'    |game_winning_goals                       |integer   |Game-winning goals.                      |
#'    |plus_minus                               |integer   |Plus/minus rating.                       |
#'    |power_play_goals                         |integer   |Power play goals.                        |
#'    |shorthanded_goals                        |integer   |Shorthanded goals.                       |
#'    |shots                                    |integer   |Shots on goal.                           |
#'    |avg_toi                                  |character |Average time on ice.                     |
#'    |faceoff_winning_pctg                     |numeric   |Faceoff winning percentage.              |
#'    |ot_goals                                 |integer   |Overtime goals.                          |
#'    |power_play_points                        |integer   |Power play points.                       |
#'    |shooting_pctg                            |numeric   |Shooting percentage.                     |
#'    |shorthanded_points                       |integer   |Shorthanded points.                      |
#'    |team_name_default                        |character |Team name (default locale).              |
#'    |team_name_cs                             |character |Team name (Czech locale).                |
#'    |team_name_de                             |character |Team name (German locale).               |
#'    |team_name_es                             |character |Team name (Spanish locale).              |
#'    |team_name_fi                             |character |Team name (Finnish locale).              |
#'    |team_name_sk                             |character |Team name (Slovak locale).               |
#'    |team_name_sv                             |character |Team name (Swedish locale).              |
#'    |team_name_fr                             |character |Team name (French locale).               |
#'    |team_common_name_default                 |character |Team common name (default locale).       |
#'    |team_common_name_cs                      |character |Team common name (Czech locale).         |
#'    |team_common_name_de                      |character |Team common name (German locale).        |
#'    |team_common_name_es                      |character |Team common name (Spanish locale).       |
#'    |team_common_name_fi                      |character |Team common name (Finnish locale).       |
#'    |team_common_name_sk                      |character |Team common name (Slovak locale).        |
#'    |team_common_name_sv                      |character |Team common name (Swedish locale).       |
#'    |team_place_name_with_preposition_default |character |Team place name with preposition.        |
#'    |team_place_name_with_preposition_fr      |character |Team place name with preposition (FR).   |
#' @keywords NHL Helpers Aggregator
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom dplyr bind_cols tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_player_career_stats(player_id = 8478402))
#' }
nhl_player_career_stats <- function(player_id) {
    url <- glue::glue(
        "https://api-web.nhle.com/v1/player/{player_id}/landing"
    )

    tryCatch(
        expr = {
            res <- .retry_request(url)
            check_status(res)

            resp_text <- .resp_text(res)
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            season_totals <- raw$seasonTotals
            if (
                is.null(season_totals) ||
                    length(season_totals) == 0 ||
                    (is.data.frame(season_totals) && nrow(season_totals) == 0)
            ) {
                message(glue::glue(
                    "{Sys.time()}: No seasonTotals data for player {player_id}"
                ))
                return(NULL)
            }

            # Ensure a data.frame (it usually is when flatten = TRUE)
            if (!is.data.frame(season_totals)) {
                season_totals <- jsonlite::fromJSON(
                    jsonlite::toJSON(season_totals, auto_unbox = TRUE),
                    flatten = TRUE
                )
            }

            season_totals <- janitor::clean_names(season_totals)

            first_name <- if (!is.null(raw$firstName$default)) {
                raw$firstName$default
            } else {
                NA_character_
            }
            last_name <- if (!is.null(raw$lastName$default)) {
                raw$lastName$default
            } else {
                NA_character_
            }
            position <- if (!is.null(raw$position)) {
                raw$position
            } else {
                NA_character_
            }

            n <- nrow(season_totals)
            bio <- dplyr::tibble(
                player_id = rep(
                    as.integer(player_id),
                    n
                ),
                first_name = rep(first_name, n),
                last_name = rep(last_name, n),
                position = rep(position, n)
            )

            df <- dplyr::bind_cols(bio, season_totals)

            df <- make_fastRhockey_data(
                df,
                "NHL Player Career Stats",
                Sys.time()
            )
            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching career stats for player {player_id}: {e$message}"
            ))
            return(NULL)
        }
    )
}
