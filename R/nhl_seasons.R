#' @title **NHL Seasons**
#' @description Returns a list of all NHL seasons with their metadata.
#' @return Returns a data frame with season information.
#' @keywords NHL Seasons
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'   try(nhl_seasons())
#' }
nhl_seasons <- function() {
    url <- "https://api-web.nhle.com/v1/season"

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            if (is.null(raw) || length(raw) == 0) {
                message(glue::glue("{Sys.time()}: No season data"))
                return(NULL)
            }

            # raw is typically a vector of season integers
            if (is.atomic(raw)) {
                df <- dplyr::tibble(season_id = raw)
            } else if (is.data.frame(raw)) {
                df <- raw
            } else {
                df <- dplyr::as_tibble(raw)
            }

            df <- df %>%
                make_fastRhockey_data("NHL Seasons", Sys.time())

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching seasons: {e$message}"
            ))
            return(NULL)
        }
    )
}


#' @title **NHL Draft Rankings**
#' @description Returns NHL draft rankings for a given season and category.
#' @param season Integer 4-digit year (e.g., 2024). If NULL, returns current.
#' @param category Integer category: 1 = North American skaters, 2 = International
#'   skaters, 3 = North American goalies, 4 = International goalies.
#'   Default 1.
#' @return Returns a data frame with draft ranking data.
#' @keywords NHL Draft Rankings
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_draft_rankings())
#' }
nhl_draft_rankings <- function(season = NULL, category = 1) {
    if (is.null(season)) {
        url <- glue::glue(
            "https://api-web.nhle.com/v1/draft/rankings/now"
        )
    } else {
        url <- glue::glue(
            "https://api-web.nhle.com/v1/draft/rankings/{season}/{category}"
        )
    }

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            if (is.null(raw) || length(raw) == 0) {
                message(glue::glue("{Sys.time()}: No draft ranking data"))
                return(NULL)
            }

            # Parse rankings out of response
            if (!is.null(raw$rankings) && is.data.frame(raw$rankings)) {
                df <- raw$rankings
            } else if (!is.null(raw$rankings)) {
                df <- jsonlite::fromJSON(
                    jsonlite::toJSON(raw$rankings, auto_unbox = TRUE),
                    flatten = TRUE
                )
            } else if (is.data.frame(raw)) {
                df <- raw
            } else {
                df <- dplyr::as_tibble(raw)
            }

            df <- df %>%
                janitor::clean_names() %>%
                make_fastRhockey_data("NHL Draft Rankings", Sys.time())

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching draft rankings: {e$message}"
            ))
            return(NULL)
        }
    )
}
