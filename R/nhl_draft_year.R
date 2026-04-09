#' @title **NHL Draft by Year**
#' @description Returns draft pick information for a given year.
#'
#' Uses the NHL API endpoint at
#' \code{api-web.nhle.com/v1/draft/picks/{year}/{round}}.
#'
#' When `round` is `NULL` (default) or the literal string `"all"`, the
#' function takes the fast path and hits
#' \code{draft/picks/{year}/all} in a single request instead of looping over
#' rounds 1-7.
#'
#' @param year Integer. Draft year (e.g. 2023).
#' @param round Integer, character, or NULL. Specific round (1-7), the
#'   literal string `"all"`, or `NULL` (treated the same as `"all"`) to
#'   fetch every round at once.
#' @return Returns a data frame of draft picks.
#' @keywords NHL Draft Year
#' @importFrom jsonlite read_json
#' @importFrom janitor clean_names
#' @importFrom dplyr bind_rows
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'    try(nhl_draft_year(year = 2023, round = 1))
#'    try(nhl_draft_year(year = 2023, round = "all"))
#' }
nhl_draft_year <- function(year, round = NULL) {
  tryCatch(
    expr = {
      all_rounds <- is.null(round) ||
        (is.character(round) && tolower(round) == "all")

      if (all_rounds) {
        # Fast path: fetch every round in a single request
        url <- glue::glue(
          "https://api-web.nhle.com/v1/draft/picks/{year}/all"
        )
        raw <- jsonlite::read_json(url, simplifyVector = TRUE)
        picks <- raw[["picks"]]
      } else {
        # Fetch a single numeric round
        url <- glue::glue(
          "https://api-web.nhle.com/v1/draft/picks/{year}/{round}"
        )
        raw <- jsonlite::read_json(url, simplifyVector = TRUE)
        picks <- raw[["picks"]]
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
