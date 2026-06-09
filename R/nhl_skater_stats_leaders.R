#' @title **NHL Skater Stats Leaders**
#' @description Returns league-wide skater statistical leaders for a given season
#' and game type. Supports multiple stat categories.
#' @param season Integer 4-digit year (e.g., 2024 for the 2024-25 season).
#'   If NULL, returns current season leaders.
#' @param game_type Integer game type: 2 = regular season (default), 3 = playoffs
#' @param categories Character vector of stat categories (e.g., "goals", "assists",
#'   "points", "plusMinus", "penaltyMins"). If NULL, returns all available categories.
#' @param limit Integer maximum number of leaders per category. If NULL, uses API default.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name           |types     |description                |
#'    |:------------------|:---------|:--------------------------|
#'    |id                 |integer   |Unique player identifier.  |
#'    |sweater_number     |integer   |Jersey number.             |
#'    |headshot           |character |URL to the player headshot image. |
#'    |team_abbrev        |character |Team abbreviation.         |
#'    |team_logo          |character |URL to the team logo image. |
#'    |position           |character |Player position.           |
#'    |value              |numeric   |Statistical value for the category. |
#'    |first_name_default |character |Player first name (default). |
#'    |last_name_default  |character |Player last name (default). |
#'    |team_name_default  |character |Team name (default).       |
#'    |category           |character |Stat leader category.      |
#'    |first_name_cs      |character |Player first name (Czech). |
#'    |first_name_de      |character |Player first name (German). |
#'    |first_name_es      |character |Player first name (Spanish). |
#'    |first_name_fi      |character |Player first name (Finnish). |
#'    |first_name_sk      |character |Player first name (Slovak). |
#'    |first_name_sv      |character |Player first name (Swedish). |
#'    |last_name_cs       |character |Player last name (Czech).  |
#'    |last_name_sk       |character |Player last name (Slovak). |
#'    |last_name_fi       |character |Player last name (Finnish). |
#'    |team_name_fr       |character |Team name (French).        |
#' @keywords NHL Skater Stats Leaders
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_skater_stats_leaders())
#' }
nhl_skater_stats_leaders <- function(
    season = NULL,
    game_type = 2,
    categories = NULL,
    limit = NULL
) {
    if (is.null(season)) {
        url <- "https://api-web.nhle.com/v1/skater-stats-leaders/current"
    } else {
        api_season <- paste0(season, season + 1)
        url <- glue::glue(
            "https://api-web.nhle.com/v1/skater-stats-leaders/{api_season}/{game_type}"
        )
    }

    # Add query parameters
    params <- list()
    if (!is.null(categories)) {
        params$categories <- paste(categories, collapse = ",")
    }
    if (!is.null(limit)) {
        params$limit <- limit
    }

    tryCatch(
        expr = {
            res <- .retry_request(url, params = params)
            check_status(res)

            resp_text <- .resp_text(res)
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            # The response has named elements per category, each with a list of leaders
            result_frames <- list()
            category_names <- setdiff(names(raw), c("season", "gameType"))

            for (cat in category_names) {
                cat_data <- raw[[cat]]
                if (!is.null(cat_data) && length(cat_data) > 0) {
                    cat_df <- jsonlite::fromJSON(
                        jsonlite::toJSON(cat_data, auto_unbox = TRUE),
                        flatten = TRUE
                    )
                    cat_df$category <- cat
                    result_frames[[cat]] <- cat_df
                }
            }

            if (length(result_frames) == 0) {
                message(glue::glue("{Sys.time()}: No skater leader data found"))
                return(NULL)
            }

            leaders_df <- dplyr::bind_rows(result_frames)
            leaders_df <- leaders_df %>%
                janitor::clean_names() %>%
                make_fastRhockey_data("NHL Skater Stats Leaders", Sys.time())

            return(leaders_df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching skater stats leaders: {e$message}"
            ))
            return(NULL)
        }
    )
}
