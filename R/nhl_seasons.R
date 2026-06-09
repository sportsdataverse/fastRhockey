#' @title **NHL Seasons**
#' @description Returns a list of all NHL seasons with their metadata.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name  |types   |description                |
#'    |:---------|:-------|:--------------------------|
#'    |season_id |integer |Season identifier (e.g., 20232024). |
#' @keywords NHL Seasons
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
            res <- .retry_request(url)
            check_status(res)

            resp_text <- .resp_text(res)
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
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name             |types     |description                |
#'    |:--------------------|:---------|:--------------------------|
#'    |last_name            |character |Prospect last name.        |
#'    |first_name           |character |Prospect first name.       |
#'    |position_code        |character |Player position code.      |
#'    |shoots_catches       |character |Handedness (shoots/catches).|
#'    |height_in_inches     |integer   |Height in inches.          |
#'    |weight_in_pounds     |integer   |Weight in pounds.          |
#'    |last_amateur_club    |character |Most recent amateur club.  |
#'    |last_amateur_league  |character |Most recent amateur league.|
#'    |birth_date           |character |Date of birth.             |
#'    |birth_city           |character |City of birth.             |
#'    |birth_state_province |character |State or province of birth.|
#'    |birth_country        |character |Country of birth.          |
#'    |midterm_rank         |integer   |Midterm draft ranking.     |
#'    |final_rank           |integer   |Final draft ranking.       |
#' @keywords NHL Draft Rankings
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
            res <- .retry_request(url)
            check_status(res)

            resp_text <- .resp_text(res)
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
