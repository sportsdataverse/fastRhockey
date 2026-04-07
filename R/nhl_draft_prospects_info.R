#' @title **NHL Draft Prospects Info**
#' @description Returns draft prospect rankings for a given year and prospect category.
#'
#' Uses the new NHL API endpoint at
#' \code{api-web.nhle.com/v1/draft/rankings/{year}/{prospect_category}}.
#'
#' The original per-prospect-ID endpoint is no longer available. This function
#' now returns rankings filtered by year and category.
#'
#' @param year Integer. Draft year (e.g. 2024).
#' @param prospect_category Integer. Prospect category:
#'   \itemize{
#'     \item 1 = North American Skater
#'     \item 2 = International Skater
#'     \item 3 = North American Goalie
#'     \item 4 = International Goalie
#'   }
#' @return Returns a data frame of draft prospect rankings.
#' @keywords NHL Draft Prospect Info
#' @importFrom jsonlite read_json
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'    try(nhl_draft_prospects_info(year = 2024, prospect_category = 1))
#' }
nhl_draft_prospects_info <- function(year, prospect_category = 1) {
  url <- glue::glue(
    "https://api-web.nhle.com/v1/draft/rankings/{year}/{prospect_category}"
  )

  tryCatch(
    expr = {
      raw <- jsonlite::read_json(url, simplifyVector = TRUE)
      rankings <- NULL
      for (key in c("rankings", "prospects", "data")) {
        if (!is.null(raw[[key]])) {
          rankings <- raw[[key]]
          break
        }
      }
      if (is.null(rankings)) {
        if (is.data.frame(raw)) {
          rankings <- raw
        } else if (is.list(raw) && length(raw) > 0) {
          rankings <- tryCatch(
            as.data.frame(raw, stringsAsFactors = FALSE),
            error = function(e2) NULL
          )
        }
      }
      if (is.null(rankings) || (is.data.frame(rankings) && nrow(rankings) == 0)) {
        message(glue::glue(
          "{Sys.time()}: No draft prospects data for year={year}, category={prospect_category} available!"
        ))
        return(NULL)
      }
      draft_prospects_df <- as.data.frame(rankings)
      draft_prospects_df <- draft_prospects_df %>%
        janitor::clean_names() %>%
        make_fastRhockey_data("NHL Draft Prospects Information from NHL.com", Sys.time())
      return(draft_prospects_df)
    },
    error = function(e) {
      message(glue::glue(
        "{Sys.time()}: Invalid arguments or no draft prospects data for year={year}, category={prospect_category} available!"
      ))
      return(NULL)
    }
  )
}
