#' @title **NHL Goalie Stats Leaders**
#' @description Returns league-wide goalie statistical leaders for a given season
#' and game type. Supports multiple stat categories.
#' @param season Integer 4-digit year (e.g., 2024 for the 2024-25 season).
#'   If NULL, returns current season leaders.
#' @param game_type Integer game type: 2 = regular season (default), 3 = playoffs
#' @param categories Character vector of stat categories (e.g., "wins",
#'   "gaa", "savePctg", "shutouts"). If NULL, returns all available categories.
#' @param limit Integer maximum number of leaders per category. If NULL, uses API default.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name           |types     |description                              |
#'    |:------------------|:---------|:----------------------------------------|
#'    |id                 |integer   |Unique player identifier.                |
#'    |sweater_number     |integer   |Jersey number.                           |
#'    |headshot           |character |Player headshot image URL.               |
#'    |team_abbrev        |character |Team abbreviation.                       |
#'    |team_logo          |character |Team logo URL.                           |
#'    |position           |character |Player position.                         |
#'    |value              |numeric   |Statistical value for the category.      |
#'    |first_name_default |character |Player first name (default locale).      |
#'    |last_name_default  |character |Player last name (default locale).       |
#'    |team_name_default  |character |Team name (default locale).              |
#'    |first_name_cs      |character |Player first name (Czech locale).        |
#'    |first_name_sk      |character |Player first name (Slovak locale).       |
#'    |last_name_cs       |character |Player last name (Czech locale).         |
#'    |last_name_sk       |character |Player last name (Slovak locale).        |
#'    |category           |character |Stat leader category.                    |
#'    |last_name_fi       |character |Player last name (Finnish locale).       |
#'    |team_name_fr       |character |Team name (French locale).               |
#' @keywords NHL Goalie Stats Leaders
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_goalie_stats_leaders())
#' }
nhl_goalie_stats_leaders <- function(
    season = NULL,
    game_type = 2,
    categories = NULL,
    limit = NULL
) {
    if (is.null(season)) {
        url <- "https://api-web.nhle.com/v1/goalie-stats-leaders/current"
    } else {
        api_season <- paste0(season, season + 1)
        url <- glue::glue(
            "https://api-web.nhle.com/v1/goalie-stats-leaders/{api_season}/{game_type}"
        )
    }

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
                message(glue::glue("{Sys.time()}: No goalie leader data found"))
                return(NULL)
            }

            leaders_df <- dplyr::bind_rows(result_frames)
            leaders_df <- leaders_df %>%
                janitor::clean_names() %>%
                make_fastRhockey_data("NHL Goalie Stats Leaders", Sys.time())

            return(leaders_df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching goalie stats leaders: {e$message}"
            ))
            return(NULL)
        }
    )
}
