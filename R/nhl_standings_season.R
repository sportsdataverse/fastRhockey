#' @title **NHL Standings Season List**
#' @description Returns the list of seasons for which standings data is available.
#' @return Returns a data frame with available standings seasons.
#' @keywords NHL Standings Season
#' @importFrom httr RETRY content
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
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
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
