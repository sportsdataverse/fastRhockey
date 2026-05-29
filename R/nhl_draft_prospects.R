#' @title **NHL Draft Prospects**
#' @description Returns current draft prospect rankings.
#'
#' Uses the new NHL API endpoint at
#' `api-web.nhle.com/v1/draft/rankings/now`.
#'
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name             |types     |description                              |
#'    |:--------------------|:---------|:----------------------------------------|
#'    |last_name            |character |Prospect's last name.                    |
#'    |first_name           |character |Prospect's first name.                   |
#'    |position_code        |character |Prospect's position code.                |
#'    |shoots_catches       |character |Prospect's shooting/catching hand.       |
#'    |height_in_inches     |integer   |Prospect's height in inches.             |
#'    |weight_in_pounds     |integer   |Prospect's weight in pounds.             |
#'    |last_amateur_club    |character |Prospect's most recent amateur club.     |
#'    |last_amateur_league  |character |Prospect's most recent amateur league.   |
#'    |birth_date           |character |Prospect's birth date.                   |
#'    |birth_city           |character |Prospect's birth city.                   |
#'    |birth_state_province |character |Prospect's birth state or province.      |
#'    |birth_country        |character |Prospect's birth country.                |
#'    |midterm_rank         |integer   |Prospect's midterm draft ranking.        |
#'    |final_rank           |integer   |Prospect's final draft ranking.          |
#' @keywords NHL Draft Prospects
#' @importFrom jsonlite read_json
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'    try(nhl_draft_prospects())
#' }
nhl_draft_prospects <- function() {
  url <- "https://api-web.nhle.com/v1/draft/rankings/now"

  tryCatch(
    expr = {
      raw <- jsonlite::read_json(url, simplifyVector = TRUE)
      # The response may have different structures depending on the API version
      # Try common keys
      rankings <- NULL
      for (key in c("rankings", "prospects", "data")) {
        if (!is.null(raw[[key]])) {
          rankings <- raw[[key]]
          break
        }
      }
      if (is.null(rankings)) {
        # If no known key, try to use the raw response if it's a data frame-like list
        if (is.data.frame(raw)) {
          rankings <- raw
        } else if (is.list(raw) && length(raw) > 0) {
          # Try to flatten the first list element
          rankings <- tryCatch(
            as.data.frame(raw, stringsAsFactors = FALSE),
            error = function(e2) NULL
          )
        }
      }
      if (is.null(rankings) || (is.data.frame(rankings) && nrow(rankings) == 0)) {
        message(glue::glue("{Sys.time()}: No draft prospects data available!"))
        return(NULL)
      }
      draft_prospects_df <- as.data.frame(rankings)
      draft_prospects_df <- draft_prospects_df %>%
        janitor::clean_names() %>%
        make_fastRhockey_data("NHL Draft Prospects data from NHL.com", Sys.time())
      return(draft_prospects_df)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no draft prospects data available!"))
      return(NULL)
    }
  )
}
