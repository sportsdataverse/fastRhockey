#' @title **NHL Stats API — Draft Summaries**
#' @description Queries the NHL Stats REST API for draft-year summaries
#'   (draft year and number of rounds). Optionally filter to a specific
#'   draft year via \code{draft_year}.
#' @param draft_year Integer draft year (e.g., 2024). If NULL, returns all
#'   draft years (1963-present).
#' @param limit Integer maximum number of results. Default 100.
#' @param start Integer start index for pagination. Default 0.
#' @param lang Character language code. Default "en".
#' @return Returns a data frame with columns: id, draft_year, rounds.
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
#' @return Returns a data frame with season data.
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
#' @description Queries miscellaneous NHL Stats REST API endpoints such as
#' glossary, game types, shift charts, and component season.
#' @param endpoint Character endpoint path, one of:
#'   "glossary", "gameType", "shiftcharts", "componentSeason"
#' @param game_id Optional game ID (required for "shiftcharts")
#' @param lang Character language code. Default "en".
#' @return Returns a data frame or list with the requested data.
#' @keywords NHL Stats Miscellaneous
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_stats_misc(endpoint = "glossary"))
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
