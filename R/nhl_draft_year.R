#' @title **NHL Draft by Year**
#' @description Returns draft pick information for a given year.
#'
#' Uses the NHL API endpoint at
#' \code{api-web.nhle.com/v1/draft/picks/{year}/{round}}.
#' When no round is specified, all rounds (1-7) are fetched and combined.
#'
#' @param year Integer. Draft year (e.g. 2023).
#' @param round Integer or NULL. Specific round (1-7) or NULL for all rounds.
#' @return Returns a data frame of draft picks.
#' @keywords NHL Draft Year
#' @importFrom jsonlite read_json
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'    try(nhl_draft_year(year = 2023, round = 1))
#' }
nhl_draft_year <- function(year, round = NULL) {
  tryCatch(
    expr = {
      if (!is.null(round)) {
        # Fetch a single round
        url <- glue::glue("https://api-web.nhle.com/v1/draft/picks/{year}/{round}")
        raw <- jsonlite::read_json(url, simplifyVector = TRUE)
        picks <- raw[["picks"]]
      } else {
        # Fetch all rounds (1-7)
        picks <- data.frame()
        for (r in 1:7) {
          url <- glue::glue("https://api-web.nhle.com/v1/draft/picks/{year}/{r}")
          tryCatch(
            expr = {
              raw <- jsonlite::read_json(url, simplifyVector = TRUE)
              round_picks <- raw[["picks"]]
              if (!is.null(round_picks) && length(round_picks) > 0) {
                picks <- dplyr::bind_rows(picks, as.data.frame(round_picks))
              }
            },
            error = function(e) {
              # Round may not exist, skip silently
            }
          )
        }
      }

      if (is.null(picks) || (is.data.frame(picks) && nrow(picks) == 0) || length(picks) == 0) {
        message(glue::glue("{Sys.time()}: No draft data for {year} available!"))
        return(NULL)
      }

      draft_df <- as.data.frame(picks)
      draft_df <- draft_df %>%
        janitor::clean_names() %>%
        make_fastRhockey_data("NHL Draft Year data from NHL.com", Sys.time())
      return(draft_df)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no draft data for {year} available!"))
      return(NULL)
    }
  )
}
