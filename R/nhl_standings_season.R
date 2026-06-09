#' @title **NHL Standings Season List**
#' @description Returns the list of seasons for which standings data is available.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                 |types     |description                |
#'    |:------------------------|:---------|:--------------------------|
#'    |id                       |integer   |Season identifier (e.g., 20232024). |
#'    |conferences_in_use       |logical   |Whether conferences were in use that season. |
#'    |divisions_in_use         |logical   |Whether divisions were in use that season. |
#'    |point_for_o_tloss_in_use |logical   |Whether a point for overtime losses was in use. |
#'    |regulation_wins_in_use   |logical   |Whether regulation wins were tracked. |
#'    |row_in_use               |logical   |Whether regulation/overtime wins (ROW) were in use. |
#'    |standings_end            |character |End date of the standings period. |
#'    |standings_start          |character |Start date of the standings period. |
#'    |ties_in_use              |logical   |Whether ties were in use that season. |
#'    |wildcard_in_use          |logical   |Whether wildcard standings were in use. |
#' @keywords NHL Standings Season
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr tibble
#' @export
#' @examples
#' \donttest{
#'   try(nhl_standings_season())
#' }
nhl_standings_season <- function() {
    url <- "https://api-web.nhle.com/v1/standings-season"

    tryCatch(
        expr = {
            res <- .retry_request(url)
            check_status(res)

            resp_text <- .resp_text(res)
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            if (is.null(raw) || length(raw) == 0) {
                message(glue::glue("{Sys.time()}: No standings season data"))
                return(NULL)
            }

            # raw$seasons is a list of objects with id and standingsStart/End
            if (!is.null(raw$seasons) && is.data.frame(raw$seasons)) {
                df <- raw$seasons
            } else if (!is.null(raw$seasons)) {
                df <- jsonlite::fromJSON(
                    jsonlite::toJSON(raw$seasons, auto_unbox = TRUE),
                    flatten = TRUE
                )
            } else if (is.data.frame(raw)) {
                df <- raw
            } else {
                df <- dplyr::as_tibble(raw)
            }

            df <- df %>%
                janitor::clean_names() %>%
                make_fastRhockey_data("NHL Standings Season", Sys.time())

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching standings season: {e$message}"
            ))
            return(NULL)
        }
    )
}
