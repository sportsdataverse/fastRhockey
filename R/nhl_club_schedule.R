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
#' @return Returns a data frame with schedule data.
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
