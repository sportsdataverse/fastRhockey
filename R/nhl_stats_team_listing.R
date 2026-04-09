#' @title **NHL Stats API — Team Listing**
#' @description Returns the top-level team listing from the NHL Stats REST
#'   API. When `team_id` is `NULL` (default) this hits
#'   `https://api.nhle.com/stats/rest/{lang}/team` and returns the full
#'   league-wide listing; when `team_id` is supplied it hits
#'   `https://api.nhle.com/stats/rest/{lang}/team/id/{team_id}` and returns a
#'   single-team payload. Distinct from [nhl_stats_teams()], which hits the
#'   per-report stats endpoints.
#' @param team_id Optional integer team id. If supplied, the by-id endpoint
#'   is used and pagination params are ignored.
#' @param lang Character language code. Default `"en"`.
#' @param limit Integer maximum number of results. Default 100. Ignored
#'   when `team_id` is supplied.
#' @param start Integer pagination start index. Default 0. Ignored when
#'   `team_id` is supplied.
#' @return A `fastRhockey_data` tibble of teams, or `NULL` on failure.
#' @keywords NHL Stats Team Listing
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \donttest{
#'   try(nhl_stats_team_listing())
#'   try(nhl_stats_team_listing(team_id = 10))
#' }
nhl_stats_team_listing <- function(
    team_id = NULL,
    lang = "en",
    limit = 100,
    start = 0
) {
    if (!is.null(team_id)) {
        url <- glue::glue(
            "https://api.nhle.com/stats/rest/{lang}/team/id/{team_id}"
        )
    } else {
        base_url <- glue::glue(
            "https://api.nhle.com/stats/rest/{lang}/team"
        )
        url <- paste0(base_url, "?start=", start, "&limit=", limit)
    }

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            if (
                is.null(raw$data) ||
                    (is.data.frame(raw$data) && nrow(raw$data) == 0)
            ) {
                message(glue::glue("{Sys.time()}: No team listing data"))
                return(NULL)
            }

            df <- raw$data %>%
                janitor::clean_names() %>%
                make_fastRhockey_data(
                    "NHL Stats Team Listing",
                    Sys.time()
                )

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching team listing: {e$message}"
            ))
            return(NULL)
        }
    )
}
