#' @title **NHL Player Career Stats**
#' @description Aggregator helper that combines biographical information
#'   from the NHL player landing endpoint with the player's season-by-season
#'   career totals. Returns a single multi-row "career-by-season" data frame.
#'   Mirrors the `Stats.player_career_stats` convenience helper from the
#'   `nhl-api-py` Python client.
#' @param player_id Integer player ID (e.g. `8478402` for Connor McDavid).
#' @return A `fastRhockey_data` / `data.frame` with one row per season
#'   played. Always includes `player_id`, `first_name`, `last_name`,
#'   `position`, `season`, `game_type_id`, plus the season stat columns
#'   exposed by the landing endpoint's `seasonTotals` payload (e.g.
#'   `goals`, `assists`, `points`, `games_played`, `pim`, `shots`,
#'   `team_name_default`, `league_abbrev`, ...). Returns `NULL` on failure.
#' @keywords NHL Helpers Aggregator
#' @importFrom httr RETRY content
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
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
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
