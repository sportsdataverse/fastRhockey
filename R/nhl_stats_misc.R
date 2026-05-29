#' @title **NHL Stats API — Draft Summaries**
#' @description Queries the NHL Stats REST API for draft-year summaries
#'   (draft year and number of rounds). Optionally filter to a specific
#'   draft year via \code{draft_year}.
#' @param draft_year Integer draft year (e.g., 2024). If NULL, returns all
#'   draft years (1963-present).
#' @param limit Integer maximum number of results. Default 100.
#' @param start Integer start index for pagination. Default 0.
#' @param lang Character language code. Default "en".
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name   |types     |description                          |
#'    |:----------|:---------|:------------------------------------|
#'    |id         |integer   |Unique draft summary identifier.     |
#'    |draft_year |integer   |Draft year.                          |
#'    |rounds     |integer   |Number of rounds in the draft.       |
#' @keywords NHL Stats Draft
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_stats_draft())
#'   try(nhl_stats_draft(draft_year = 2024))
#' }
nhl_stats_draft <- function(
    draft_year = NULL,
    limit = 100,
    start = 0,
    lang = "en"
) {
    base_url <- glue::glue(
        "https://api.nhle.com/stats/rest/{lang}/draft"
    )

    url <- paste0(base_url, "?start=", start, "&limit=", limit)

    if (!is.null(draft_year)) {
        cayenne <- paste0("draftYear=", as.integer(draft_year))
        url <- paste0(
            url,
            "&cayenneExp=",
            utils::URLencode(cayenne, reserved = TRUE)
        )
    }

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            if (is.null(raw$data) || nrow(raw$data) == 0) {
                message(glue::glue("{Sys.time()}: No draft stats data"))
                return(NULL)
            }

            df <- raw$data
            df <- df %>%
                janitor::clean_names() %>%
                make_fastRhockey_data("NHL Stats Draft", Sys.time())

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching draft stats: {e$message}"
            ))
            return(NULL)
        }
    )
}


#' @title **NHL Stats API — Seasons List**
#' @description Returns a list of all seasons from the Stats REST API.
#' @param lang Character language code. Default "en".
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                                          |types     |description                                                       |
#'    |:-------------------------------------------------|:---------|:-----------------------------------------------------------------|
#'    |id                                                |integer   |Season identifier (YYYYYYYY).                                     |
#'    |all_star_game_in_use                              |integer   |Whether an All-Star Game was held this season.                    |
#'    |conferences_in_use                                |integer   |Whether conferences were in use this season.                      |
#'    |divisions_in_use                                  |integer   |Whether divisions were in use this season.                        |
#'    |end_date                                          |character |Season end date.                                                  |
#'    |entry_draft_in_use                                |integer   |Whether an entry draft was in use this season.                    |
#'    |formatted_season_id                               |character |Human-readable season string (e.g. "2023-24").                    |
#'    |minimum_playoff_minutes_for_goalie_stats_leaders  |integer   |Minimum playoff minutes to qualify for goalie stats leaders.      |
#'    |minimum_regular_games_for_goalie_stats_leaders    |integer   |Minimum regular-season games to qualify for goalie stats leaders. |
#'    |nhl_stanley_cup_owner                             |integer   |Whether the NHL owned the Stanley Cup this season.                |
#'    |number_of_games                                   |integer   |Number of games per team this season.                             |
#'    |olympics_participation                            |integer   |Whether NHL players participated in the Olympics this season.     |
#'    |point_for_ot_loss_in_use                          |integer   |Whether the overtime-loss point was in use this season.           |
#'    |preseason_startdate                               |character |Preseason start date.                                             |
#'    |regular_season_end_date                           |character |Regular-season end date.                                          |
#'    |row_in_use                                         |integer   |Whether the regulation/overtime/shootout format was in use.       |
#'    |season_ordinal                                    |integer   |Ordinal sequence number of the season.                            |
#'    |start_date                                        |character |Season start date.                                                |
#'    |supplemental_draft_in_use                         |integer   |Whether a supplemental draft was in use this season.              |
#'    |ties_in_use                                       |integer   |Whether ties were in use this season.                             |
#'    |total_playoff_games                               |integer   |Total number of playoff games this season.                        |
#'    |total_regular_season_games                        |integer   |Total number of regular-season games this season.                 |
#'    |wildcard_in_use                                   |integer   |Whether the wild-card playoff format was in use this season.      |
#' @keywords NHL Stats Seasons
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_stats_seasons())
#' }
nhl_stats_seasons <- function(lang = "en") {
    url <- glue::glue(
        "https://api.nhle.com/stats/rest/{lang}/season"
    )

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            if (is.null(raw$data) || length(raw$data) == 0) {
                message(glue::glue(
                    "{Sys.time()}: No season data from Stats API"
                ))
                return(NULL)
            }

            df <- raw$data
            df <- df %>%
                janitor::clean_names() %>%
                make_fastRhockey_data("NHL Stats Seasons", Sys.time())

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching stats seasons: {e$message}"
            ))
            return(NULL)
        }
    )
}


#' @title **NHL Stats API — Miscellaneous Endpoints**
#' @description Generic dispatcher for the NHL Stats REST API
#'   (`https://api.nhle.com/stats/rest/{lang}/{endpoint}`). Pass any valid
#'   endpoint name as `endpoint` and the helper will issue the request and
#'   return the parsed `data` element as a `data.frame`. For non-tabular
#'   endpoints, the raw parsed list is returned instead.
#' @param endpoint Character endpoint path. Known valid values include:
#'
#'   * **Tabular:** `"franchise"`, `"glossary"`, `"country"`, `"config"`,
#'     `"players"`, `"team"`, `"game"`, `"componentSeason"`,
#'     `"milestones/skaters"`, `"milestones/goalies"`,
#'     `"leaders/skaters/{attribute}"`, `"leaders/goalies/{attribute}"`
#'   * **Reference / lookup:** `"gameType"`, `"shiftcharts"` (use the
#'     dedicated `game_id` argument for this one),
#'     `"content/module/{templateKey}"`, `"team/id/{id}"`
#'   * **Health check:** `"ping"`
#'
#'   Note: dedicated wrappers exist for `season`, `draft`, `skater/{report}`,
#'   `goalie/{report}`, and `team/{report}` — see [nhl_stats_seasons()],
#'   [nhl_stats_draft()], [nhl_stats_skaters()], [nhl_stats_goalies()],
#'   and [nhl_stats_teams()].
#' @param game_id Optional game ID (required for `endpoint = "shiftcharts"`).
#' @param lang Character language code. Default `"en"`.
#' @return A data frame (`fastRhockey_data`) for tabular endpoints, or the
#'   raw parsed list for non-tabular ones. For the default `"glossary"`
#'   endpoint the columns are:
#'
#'    |col_name              |types     |description                                       |
#'    |:---------------------|:---------|:-------------------------------------------------|
#'    |id                    |integer   |Unique glossary entry identifier.                 |
#'    |abbreviation          |character |Stat abbreviation.                                |
#'    |definition            |character |Definition of the stat or term.                   |
#'    |first_season_for_stat |integer   |First season the stat was tracked (YYYYYYYY).     |
#'    |full_name             |character |Full name of the stat or term.                    |
#'    |language_code         |character |Language code of the entry.                        |
#'    |last_updated          |character |Timestamp the entry was last updated.             |
#' @keywords NHL Stats Miscellaneous
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_stats_misc(endpoint = "glossary"))
#'   try(nhl_stats_misc(endpoint = "franchise"))
#'   try(nhl_stats_misc(endpoint = "country"))
#' }
nhl_stats_misc <- function(endpoint = "glossary", game_id = NULL, lang = "en") {
    if (endpoint == "shiftcharts" && !is.null(game_id)) {
        url <- glue::glue(
            "https://api.nhle.com/stats/rest/{lang}/shiftcharts?cayenneExp=gameId={game_id}"
        )
    } else if (endpoint == "componentSeason") {
        url <- glue::glue(
            "https://api.nhle.com/stats/rest/{lang}/componentSeason"
        )
    } else {
        url <- glue::glue(
            "https://api.nhle.com/stats/rest/{lang}/{endpoint}"
        )
    }

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            if (
                !is.null(raw$data) &&
                    is.data.frame(raw$data) &&
                    nrow(raw$data) > 0
            ) {
                df <- raw$data %>%
                    janitor::clean_names() %>%
                    make_fastRhockey_data("NHL Stats Misc", Sys.time())
                return(df)
            }

            return(raw)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching stats misc '{endpoint}': {e$message}"
            ))
            return(NULL)
        }
    )
}
