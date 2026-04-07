#' @title **NHL Stats API — Team Stats**
#' @description Queries the NHL Stats REST API for team-level statistics.
#' @param report_type Character report type. Default "summary".
#'   Common types: "summary", "penalties", "penaltykill",
#'   "penaltykilltime", "powerplay", "powerplaytime",
#'   "realtime", "faceoffpercentages", "faceoffwins",
#'   "goalsForAgainst", "goalsBy Period", "daysrest",
#'   "outshootoutshotby", "percentages", "scoretrailfirst",
#'   "shootout", "shottype"
#' @param season Character season in "YYYYYYYY" format (e.g., "20242025").
#'   If NULL, uses current season.
#' @param game_type Integer game type: 2 = regular season (default), 3 = playoffs
#' @param limit Integer maximum number of results. Default 50.
#' @param start Integer start index for pagination. Default 0.
#' @param sort Character sort column. Default "points".
#' @param direction Character sort direction: "DESC" or "ASC". Default "DESC".
#' @param lang Character language code. Default "en".
#' @return Returns a data frame with team statistics.
#' @keywords NHL Stats Team
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_stats_teams())
#' }
nhl_stats_teams <- function(
    report_type = "summary",
    season = NULL,
    game_type = 2,
    limit = 50,
    start = 0,
    sort = "points",
    direction = "DESC",
    lang = "en"
) {
    if (is.null(season)) {
        s <- most_recent_nhl_season()
        season <- paste0(s, s + 1)
    }

    base_url <- glue::glue(
        "https://api.nhle.com/stats/rest/{lang}/team/{report_type}"
    )

    cayenne_exp <- glue::glue(
        "gameTypeId={game_type} and seasonId<={season} and seasonId>={season}"
    )

    sort_field <- paste0(
        '[{"property":"',
        sort,
        '","direction":"',
        direction,
        '"}]'
    )

    url <- paste0(
        base_url,
        "?",
        "isAggregate=false",
        "&isGame=false",
        "&sort=",
        utils::URLencode(sort_field, reserved = TRUE),
        "&start=",
        start,
        "&limit=",
        limit,
        "&cayenneExp=",
        utils::URLencode(cayenne_exp, reserved = TRUE)
    )

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            if (is.null(raw$data) || length(raw$data) == 0) {
                message(glue::glue("{Sys.time()}: No team stats data"))
                return(NULL)
            }

            df <- raw$data
            df <- df %>%
                janitor::clean_names() %>%
                make_fastRhockey_data("NHL Stats Teams", Sys.time())

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching team stats: {e$message}"
            ))
            return(NULL)
        }
    )
}
